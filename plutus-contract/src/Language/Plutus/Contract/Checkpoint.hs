{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Language.Plutus.Contract.Checkpoint(
    -- $checkpoints
    Checkpoint
    , CheckpointError(..)
    , AsCheckpointError(..)
    , CheckpointStore(..)
    , CheckpointKey
    , jsonCheckpoint
    , handleCheckpoint
    , handleCheckpointEnv
    -- * Misc.
    , runCheckpoint
    , runCheckpointIO
    , action1
    , action2
    ) where

import           Control.Lens
import           Control.Monad.Freer
import           Control.Monad.Freer.Error               (Error, runError, throwError)
import           Control.Monad.Freer.Extras              (raiseEnd4)
import           Control.Monad.Freer.Log                 (Log, LogMessage, logDebug, logInfo)
import           Control.Monad.Freer.State               (State, evalState, get, gets, modify, put, runState)
import           Control.Monad.Freer.Writer              (runWriter)
import           Data.Aeson                              (FromJSON, FromJSONKey, ToJSON, ToJSONKey, Value)
import qualified Data.Aeson.Types                        as JSON
import           Data.Foldable                           (traverse_)
import           Data.Map                                (Map)
import qualified Data.Map                                as Map
import qualified Data.Text                               as Text
import           Data.Text.Prettyprint.Doc               (Pretty (..), colon, defaultLayoutOptions, layoutPretty,
                                                          viaShow, vsep, (<+>))
import qualified Data.Text.Prettyprint.Doc.Render.String as Render
import           GHC.Generics                            (Generic)

-- $checkpoints
-- This module contains a checkpoints mechanism that can be used to store
-- intermediate results of 'Control.Monad.Freer.Eff' programs as JSON values
-- inside a 'CheckpointStore'. It works similar to the short-circuiting behavior
-- of 'Control.Monad.Freer.Error.Error': Before we execute an action
-- 'Eff effs a' whose result should be checkpointed, we check if the there is
-- already a value of 'a' for this checkpoint it in the store. If there is, we
-- return it *instead* of running the action. If there isn't we run the action
-- and then store the result.
-- Note that if the checkpoint is present the action will not be executed at
-- all. (That is the point of the checkpoint mechanism). If the action has any
-- side effects that you would like to repeat on restoring the value, you can
-- use the type parameter 'e' (for environment) to do this in
-- 'handleCheckpointEnv'. See 'runCheckpoint' for an example that uses the
-- environment to store the state of a 'State Int' effect.
--
-- To create a checkpoint use 'jsonCheckpoint'.
--
-- To handle the checkpoint effect use 'handleCheckpoint' or 'handleCheckpoint'.

newtype CheckpointKey = CheckpointKey Int
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON, ToJSONKey, FromJSONKey, Num, Enum, Pretty)

data CheckpointError = JSONDecodeError String
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

makeClassyPrisms ''CheckpointError

newtype CheckpointStore = CheckpointStore { unCheckpointStore :: Map CheckpointKey Value }
    deriving stock (Eq, Show, Generic)
    deriving newtype (ToJSON, FromJSON, Semigroup, Monoid)

instance Pretty CheckpointStore where
    pretty (CheckpointStore mp) =
        let p k v = pretty k <> colon <+> viaShow v in
        vsep (uncurry p <$> Map.toList mp)

_CheckpointStore :: Iso' CheckpointStore (Map CheckpointKey Value)
_CheckpointStore = iso unCheckpointStore CheckpointStore

data CheckpointStoreItem a e =
    CheckpointStoreItem
        { csValue       :: a
        , csNewKey      :: CheckpointKey
        , csEnvironment :: e
        }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Insert a new value into the checkpoint store. The first 'CheckpointKey' is
--   the checkpoint key *before* running the checkpointed action, the second
--   'Checkpoint' key is the value *after* running it. When we restore the
--   checkpoint from the state (in 'restore') we set the 'CheckpointKey' state
--   to the second argument to prevent chaos.
insert ::
    ( ToJSON a
    , ToJSON env
    , Member (State CheckpointStore) effs
    )
    => CheckpointKey
    -> CheckpointKey
    -> a
    -> env
    -> Eff effs ()
insert k k' v env =
    let vl = CheckpointStoreItem{csValue = v, csNewKey = k', csEnvironment=env}
    in modify (over _CheckpointStore (Map.insert k (JSON.toJSON vl)))

restore ::
    forall a e effs.
    ( FromJSON a
    , FromJSON e
    , Member (State CheckpointStore) effs
    , Member (State CheckpointKey) effs
    , Member Log effs
    )
    => (e -> Eff effs ())
    -> CheckpointKey
    -> Eff effs (Either CheckpointError (Maybe a))
restore putEnv k = do
    (result :: Maybe (Either String (CheckpointStoreItem a e))) <- fmap (fmap (JSON.parseEither JSON.parseJSON)) (gets (view $ _CheckpointStore . at k))
    case result of
        Nothing -> do
            logDebug $ "No value for " <> Text.pack (show k)
            pure $ Right Nothing
        Just (Left err) -> do
            logDebug $ "Decoding error at key " <> Text.pack (show k)
            pure $ Left (JSONDecodeError err)
        Just (Right CheckpointStoreItem{csValue,csNewKey, csEnvironment}) -> do
            logDebug "Found a value, restoring previous key"
            put csNewKey
            logDebug "Restoring environment"
            putEnv csEnvironment
            pure (Right (Just csValue))

data Checkpoint e r where
    DoCheckpoint :: Checkpoint e ()
    GetKey :: Checkpoint e CheckpointKey
    Store :: (ToJSON a) => CheckpointKey -> CheckpointKey -> a -> Checkpoint e ()
    Retrieve :: (FromJSON a) => CheckpointKey -> Checkpoint e (Either CheckpointError (Maybe a))

doCheckpoint :: forall e effs. Member (Checkpoint e) effs => Eff effs ()
doCheckpoint = send (DoCheckpoint @e)

getKey :: forall e effs. Member (Checkpoint e) effs => Eff effs CheckpointKey
getKey = send (GetKey @e)

store :: forall a e effs. (Member (Checkpoint e) effs, ToJSON a) => CheckpointKey -> CheckpointKey -> a -> Eff effs ()
store k1 k2 a = send @(Checkpoint e) (Store k1 k2 a)

retrieve :: forall a e effs. (Member (Checkpoint e) effs, FromJSON a) => CheckpointKey -> Eff effs (Either CheckpointError (Maybe a))
retrieve k = send @(Checkpoint e) (Retrieve k)

handleCheckpoint ::
    forall effs a.
    ( Member (State CheckpointStore) effs
    , Member (State CheckpointKey) effs
    , Member Log effs
    )
    => Eff (Checkpoint () ': effs) a
    -> Eff effs a
handleCheckpoint =
    handleCheckpointEnv @() @effs (pure ()) (\_ -> pure ())

-- | Handle the 'Checkpoint' effect in terms of a 'CheckpointStore' effect.
handleCheckpointEnv ::
    forall e effs.
    ( Member (State CheckpointStore) effs
    , Member (State CheckpointKey) effs
    , Member Log effs
    , ToJSON e
    , FromJSON e
    )
    => (Eff effs e)
    -- ^ How to get the environment that should be stored alongside checkpointed values
    -> (e -> Eff effs ())
    -- ^ How to restore the environment when a checkpoint has been restored.
    -> Eff (Checkpoint e ': effs)
    ~> Eff effs
handleCheckpointEnv getEnv storeEnv = interpret $ \case
    DoCheckpoint -> do
        logDebug "handleCheckpoint: doCheckpoint"
        modify @CheckpointKey succ
    GetKey -> do
        logDebug "handleCheckpoint: getKey"
        get @CheckpointKey
    Store k k' a -> do
        logDebug "handleCheckpoint: store"
        logDebug $ "key1: " <> Text.pack (show k)
        logDebug $ "key2: " <> Text.pack (show k')
        getEnv >>= insert k k' a
    Retrieve k -> do
        logDebug "handleCheckpoint: retrieve"
        logDebug $ "key then: " <> Text.pack (show k)
        result <- restore @_ @e @effs storeEnv k
        k' <- get @CheckpointKey
        logDebug $ "key now: " <> Text.pack (show k')
        pure result

jsonCheckpoint ::
    forall e err a effs.
    ( Member (Checkpoint e) effs
    , Member (Error err) effs
    , ToJSON a
    , FromJSON a
    , AsCheckpointError err
    )
    => Eff effs a
    -> Eff effs a
jsonCheckpoint action = do
    doCheckpoint @e
    k <- getKey @e
    vl <- retrieve @_ @e k
    case vl of
        Left err -> throwError @err (review _CheckpointError err)
        Right (Just a) -> return a
        Right Nothing -> do
            result <- action
            k' <- getKey @e
            store  @_ @e k k' result
            pure result

-- | Run the 'Checkpoint' effect (for debugging purposes)
runCheckpoint ::
    CheckpointStore
    -> Eff '[Checkpoint Int, Log, State Int, Error CheckpointError] a
    -> (Either CheckpointError (a, CheckpointStore), [LogMessage])
runCheckpoint theStore =
    run
    . runWriter
    . runError @CheckpointError
    . runState theStore
    . evalState (0 :: CheckpointKey)
    . subsume
    . evalState (0 :: Int)
    . subsume
    . handleCheckpointEnv @Int @'[Log, State Int, Error CheckpointError, State CheckpointKey, State CheckpointStore, Error CheckpointError, Log] (get @Int) (put @Int)
    . raiseEnd4

runCheckpointIO ::
    CheckpointStore
    -> Eff '[Checkpoint Int, Log, State Int, Error CheckpointError] a
    -> IO (a, CheckpointStore)
runCheckpointIO theStore action = do
    let (result, messages) = runCheckpoint theStore action
    traverse_ (putStrLn . Render.renderString . layoutPretty defaultLayoutOptions . pretty) messages
    either (error . show) pure result

action1 :: Eff '[Checkpoint Int, Log, State Int, Error CheckpointError] Int
action1 = do
    logInfo "Starting action1"
    put @Int 1
    jsonCheckpoint @Int @CheckpointError $ do
        logInfo "within checkpoint"
        put @Int 2
        jsonCheckpoint @Int @CheckpointError $ do
            logInfo "within checkpoint 2"
            put @Int 3
            jsonCheckpoint @Int @CheckpointError $ do
                logInfo "within checkpoint 3"
                put @Int 4
                pure ()
    logInfo "Done with action1"
    get

action2 :: Eff '[Checkpoint Int, Log, State Int, Error CheckpointError] Int
action2 = (+) <$> action1 <*> action1
