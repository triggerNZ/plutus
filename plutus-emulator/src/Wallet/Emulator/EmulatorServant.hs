{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Wallet.Emulator.EmulatorServant where

import           Control.Concurrent.STM.TVar  (TVar, readTVar, writeTVar)
import           Control.Lens                 hiding (index)
import           Control.Monad                (void)
import           Control.Monad.Except         (MonadError)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Reader         (MonadReader, ask)
import           Control.Monad.State.Lazy     (State)
import           Control.Monad.STM            (atomically)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Conduit                 (ConduitT)
import           Data.Conduit.Combinators     (yieldMany)
import           Data.Proxy                   (Proxy (..))
import           GHC.Generics                 (Generic)
import           Ledger                       (Tx (..))
import           Network.Wai.Handler.Warp     as Warp
import           Servant
import           Servant.API.WebSocketConduit
import           Servant.Client

import           Ledger.Blockchain            (Block, Blockchain, unspentOutputs)
import qualified Ledger.Index                 as Index
import           Ledger.Slot                  (Slot (..))
import           Wallet.Emulator.Types        (EmulatorEvent, TxPool, ValidatedBlock(..), validateBlock)

type EmulatorMonad m = (MonadReader (TVar AppState) m, MonadError ServantError m, MonadIO m)

-- Server
data EmulatorState = EmulatorState {
    _chainNewestFirst :: Blockchain,
    _txPool           :: TxPool,
    _emulatorLog      :: [EmulatorEvent]
} deriving stock (Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

data AppState = AppState {
    _emulatorState :: EmulatorState
}

emptyState :: EmulatorState
emptyState = EmulatorState {
    _chainNewestFirst = [],
    _txPool = [],
    _emulatorLog = []
}

data ProcessedBlock = ProcessedBlock {
    acceptedTxs :: [Tx],
    rejectedTxs :: [Tx]
}   deriving stock (Generic, Eq)
    deriving anyclass (ToJSON, FromJSON)

$(makeLenses ''EmulatorState)
$(makeLenses ''AppState)

publishTx :: EmulatorMonad m => Tx -> m ()
publishTx tx = do
    tState <- ask
    void . liftIO . atomically $ do
        oldState <- readTVar tState
        let newState = over (emulatorState . txPool) (tx :) oldState
        writeTVar tState newState

currentSlot :: EmulatorMonad m => m Integer
currentSlot = do
    tState <- ask
    let viewChain = view (emulatorState . chainNewestFirst)
    liftIO . atomically $ fromIntegral . length . viewChain <$> readTVar tState

txnStream :: MonadIO m => Integer -> TVar AppState -> ConduitT () Block m ()
txnStream slot state = loop slot
    where
        loop :: MonadIO m => Integer -> ConduitT () Block m ()
        loop s = do
            blocks <- liftIO . atomically $ do
                st <- readTVar state
                let chain = view (emulatorState . chainNewestFirst) st
                pure (take (fromIntegral s) chain)
            void $ yieldMany blocks
            loop $ s + fromIntegral (length blocks)

listenBlockStream :: EmulatorMonad m => Integer -> m (ConduitT () Block (ResourceT IO) ())
listenBlockStream slot = txnStream slot <$> ask

currentState :: EmulatorMonad m => m EmulatorState
currentState = _emulatorState <$> (ask >>= liftIO . atomically . readTVar)

processBlock :: EmulatorMonad m => m ProcessedBlock
processBlock = do
    tState <- ask
    liftIO . atomically $ do
        st <- readTVar tState
        let pool = view (emulatorState . txPool) st
            chain = view (emulatorState . chainNewestFirst) st
            slot  = Slot $ fromIntegral (length chain)
            index = Index.UtxoIndex $ unspentOutputs chain
            (ValidatedBlock block events rest index') =
                validateBlock slot index pool
            newChain = block : chain
        pure ProcessedBlock {
            acceptedTxs = block,
            rejectedTxs = rest
        }
        

{-
** SERVANT **

type EmulatorAPI = "publishTx" :> ReqBody '[JSON] Tx :> Post '[JSON] Tx
              :<|> "currentSlot" :> Get '[JSON] Integer
              :<|> "listenBlockStream" :> WebSocketConduit () Block
              :<|> "mock" :> "currentState" :> Get '[JSON] EmulatorState
              :<|> "mock" :> "processBlock" :> Get '[JSON] ProcessedBlock

api :: Proxy EmulatorAPI
api = Proxy

transformHandler :: (EmulatorMonad m) => AppState -> m a -> Handler a
transformHandler state handler = runReaderT handler state

emulatorServer :: Server EmulatorAPI
emulatorServer = hoistServer api (transformHandler initialState) srv
    where
        srv :: ServerT EmulatorAPI EmulatorHandler
        srv = publishTxHandler
         :<|> currentSlotHandler
         :<|> listenBlockStreamHandler
         :<|> currentStateHandler
         :<|> processBlockHandler

        initialState :: AppState
        initialState = undefined


-- Clients
publishTx    :: Tx -> ClientM Tx
currentSlot  :: ClientM Integer
--listenBlockStream :: Monad m => ClientM (ConduitT () Block m ())
currentState :: ClientM EmulatorState
processBlock :: ClientM ProcessedBlock
publishTx :<|> currentSlot :<|> listenBlockStream :<|> currentState :<|> processBlock = client api

emulatorApp :: Application
emulatorApp = serve (Proxy @EmulatorAPI) emulatorServer

main :: IO ()
main = Warp.run 8081 emulatorApp
-}
