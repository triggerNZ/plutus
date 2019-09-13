{-# LANGUAGE DeriveGeneric #-}
module Marlowe.Symbolic.Types.Response where

import           Data.Aeson hiding (Result)
import           GHC.Generics

data Result = Valid
            | CounterExample
                { initialSlot :: Integer
                , transactionList :: String
                , transactionWarning :: String
                }
  deriving (Generic)
instance FromJSON Result
instance ToJSON Result

data Response = Response
  { uuid :: String
  , result :: Result 
  } deriving (Generic)
instance FromJSON Response
instance ToJSON Response

