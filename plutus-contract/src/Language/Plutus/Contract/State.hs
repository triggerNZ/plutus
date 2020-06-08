{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Plutus.Contract.State(
    -- $contractstate
    Contract
    , State(..)
    , ContractRequest(..)
    , ContractResponse(..)
    , insertAndUpdateContract
    , initialiseContract
    ) where

import           Data.Aeson                          (FromJSON, ToJSON)
import           GHC.Generics                        (Generic)

import           Data.Text.Prettyprint.Doc.Extras    (Pretty, PrettyShow (..))
import           Language.Plutus.Contract.Checkpoint (CheckpointStore)
import           Language.Plutus.Contract.Resumable
import           Language.Plutus.Contract.Schema     (Event (..), Handlers (..))
import           Language.Plutus.Contract.Types

-- $contractstate
-- Types for initialising and running contract instances

data State e = State
    { record      :: Responses e
    , checkpoints :: CheckpointStore
    }
    deriving stock (Generic, Eq, Show, Functor, Foldable, Traversable)
    deriving anyclass (ToJSON, FromJSON)

data ContractRequest s = ContractRequest
    { oldState :: State s
    , event    :: Response s
    }
    deriving stock (Generic, Eq, Show, Functor, Foldable, Traversable)
    deriving anyclass (ToJSON, FromJSON)
    deriving Pretty via PrettyShow (ContractRequest s)

data ContractResponse s h = ContractResponse
    { newState :: State s
    , hooks    :: [Request h]
    }
    deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON)

insertAndUpdateContract ::
    forall s e a.
    Contract s e a
    -> ContractRequest (Event s)
    -> Either e (ContractResponse (Event s) (Handlers s))
insertAndUpdateContract (Contract con) ContractRequest{oldState=State record checkpoints, event} =
    mkResponse <$> insertAndUpdate con checkpoints record event

mkResponse :: forall s e a. ResumableResult s e a -> ContractResponse s e
mkResponse ResumableResult{wcsResponses, wcsRequests=Requests{unRequests},wcsCheckpointStore} =
    ContractResponse{hooks = unRequests, newState = State { record = wcsResponses, checkpoints=wcsCheckpointStore } }

initialiseContract
    :: forall s e a.
    Contract s e a
    -> Either e (ContractResponse (Event s) (Handlers s))
initialiseContract (Contract c) = mkResponse <$> runResumable [] mempty c
