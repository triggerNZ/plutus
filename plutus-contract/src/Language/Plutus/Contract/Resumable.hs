{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Language.Plutus.Contract.Resumable(
    -- * The 'Resumable' effect and its handlers.
    Resumable(..)
    , prompt
    , select
    -- ** Interpreting the 'Resumable' effect
    , handleResumable
    , handleNonDetPrompt
    , Request(..)
    , Response(..)
    , RequestID(..)
    , IterationID(..)
    , RequestState(..)
    , initialRequestState
    , ResumableInterpreter
    , Record(..)
    , insertResponse
    ) where

import           Control.Applicative
import           Data.Aeson                    (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import           Data.List.NonEmpty            (NonEmpty (..))
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Semigroup                (Max (..))
import Data.Semigroup.Foldable (foldMap1)
import           Data.Text.Prettyprint.Doc
import           GHC.Generics                  (Generic)
import           Numeric.Natural               (Natural)

import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.NonDet
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.State

data Resumable i o r where
    RRequest :: o -> Resumable i o i
    -- See https://hackage.haskell.org/package/freer-simple-1.2.1.1/docs/src/Control.Monad.Freer.Internal.html#NonDet
    RSelect :: Resumable i o Bool

prompt :: Member (Resumable i o) effs => o -> Eff effs i
prompt o = send (RRequest o)

select ::
    forall i o effs a.
    Member (Resumable i o) effs
    => Eff effs a
    -> Eff effs a
    -> Eff effs a
select l r = send @(Resumable i o) RSelect >>= \b -> if b then l else r

handleResumable ::
    forall i o effs.
    ( Member (Yield o i) effs
    , Member NonDet effs
    )
    => Eff (Resumable i o ': effs)
    ~> Eff effs
handleResumable = interpret $ \case
    RRequest o -> yield o id
    RSelect -> send MPlus

newtype RequestID = RequestID Int
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Pretty, Enum, Num)

newtype IterationID = IterationID Natural
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Pretty, Enum, Num)
    deriving (Semigroup) via (Max Natural)

instance Monoid IterationID where
    mappend = (<>)
    mempty = IterationID 0

data Request o =
    Request
        { rqID      :: RequestID
        , itID      :: IterationID
        , rqRequest :: o
        } deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
           deriving anyclass (ToJSON, FromJSON)

instance Pretty o => Pretty (Request o) where
    pretty Request{rqID, itID, rqRequest} =
        indent 2 $ vsep [
            "Iteration" <+> pretty itID <+> "request ID" <+> pretty rqID,
            "Request:" <+> pretty rqRequest
        ]

data Response i =
    Response
        { rspRqID     :: RequestID
        , rspItID     :: IterationID
        , rspResponse :: i
        }  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
           deriving anyclass (ToJSON, FromJSON)

instance Pretty i => Pretty (Response i) where
    pretty Response{rspRqID, rspItID, rspResponse} =
        indent 2 $ vsep [
            "Iteration" <+> pretty rspItID <+> "request ID" <+> pretty rspRqID,
            "Response:" <+> pretty rspResponse
        ]

data RequestState o =
    RequestState
        { rsOpenRequests :: [Request o]
        } deriving (Eq, Ord, Show)

instance Pretty o => Pretty (RequestState o) where
    pretty RequestState{rsOpenRequests} =
        indent 2 $ vsep ("Requests:" : (pretty <$> rsOpenRequests))

initialRequestState :: RequestState o
initialRequestState =
    RequestState
        { rsOpenRequests = []
        }

pruneRequests :: [Request o] -> [Request o]
pruneRequests [] = []
pruneRequests (r:rs) =
    let Max maxIteration = foldMap1 (Max . itID) (r :| rs)
    in filter ((==) maxIteration . itID) (r:rs)

nextRequestID ::
    ( Member (State (RequestState o)) effs
    , Member (State IterationID) effs
    , Member (State RequestID) effs
    )
    => o
    -> Eff effs (IterationID, RequestID)
nextRequestID s = do
    RequestState{rsOpenRequests} <- get
    requestID <- get @RequestID
    iid <- get @IterationID
    let niid = succ iid
        nid  = succ requestID
    put $ RequestState
            { rsOpenRequests = Request{rqRequest=s,rqID=nid,itID=niid} : rsOpenRequests
            }
    put niid
    put nid
    pure (niid, nid)

clearRequests :: forall o effs. Member (State (RequestState o)) effs => Eff effs ()
clearRequests = modify @(RequestState o) (\rq -> rq{rsOpenRequests = [] })

-- Effects that are used to interpret the Yield/NonDet combination
-- produced by 'handleResumable'.
type ResumableInterpreter i o effs =
    -- anything that comes *before* NonDet can be backtracked.

    -- We put 'State IterationID' here to ensure that only
    -- the 'State IterationID' effects of the branch that is
    -- selected will persist, so that the iteration ID is increased
    -- exactly once per branching level.
     State IterationID
     ': NonDet
     ': State RequestID
     ': effs

newtype Record i = Record { unRecord :: Map (IterationID, RequestID) i }
    deriving newtype (Eq, Ord, Show, Semigroup, Monoid, ToJSON, FromJSON)
    deriving stock (Generic, Functor, Foldable, Traversable)

instance Pretty i => Pretty (Record i) where
    pretty (Record mp) =
        let entries = Map.toList mp
            prettyEntry ((itID, reqID), i) =
                hang 2 $ vsep ["IterationID:" <+> pretty itID, "RequestID:" <+> pretty reqID, "Event:" <+> pretty i]
        in vsep (prettyEntry <$> entries)

insertResponse :: Response i -> Record i -> Record i
insertResponse Response{rspRqID,rspItID,rspResponse} (Record r) =
    Record $ Map.insert (rspItID, rspRqID) rspResponse r

-- Return the answer or the remaining requests
mkResult ::
    forall effs o a.
    Member (State (RequestState o)) effs
    => Maybe a
    -> Eff effs (Maybe a)
mkResult (Just a) = modify @(RequestState o) (\rs -> rs { rsOpenRequests = [] }) >> pure (Just a)
mkResult Nothing  = modify @(RequestState o) (\rs -> rs { rsOpenRequests = pruneRequests (rsOpenRequests rs)}) >> pure Nothing

-- | Interpret 'Yield' as a prompt-type effect using NonDet to
--   branch out and choose a branch, and the state effects to
--   keep track of request IDs.
handleNonDetPrompt ::
    forall i o a effs.
    ( Member (Reader (Record i)) effs
    , Member (State (RequestState o)) effs
    )
    => Eff (Yield o i ': ResumableInterpreter i o effs) a
    -> Eff effs (Maybe a)
handleNonDetPrompt e = result where
    result =
        mkResult @effs @o @a =<<
            (evalState (RequestID 0) $ makeChoiceA @Maybe $ evalState mempty $ (loop =<< runC e))

    -- Check the result and write a request to the state if
    -- the computation is waiting for input
    loop :: Status (ResumableInterpreter i o effs) o i a -> Eff (ResumableInterpreter i o effs) a
    loop (Continue a k) = do
        Record mp' <- ask
        (iid,nid) <- nextRequestID a
        case Map.lookup (iid, nid) mp' of
            Nothing -> empty
            Just v  -> clearRequests @o >> k v >>= loop
    loop (Done a)       = pure a

