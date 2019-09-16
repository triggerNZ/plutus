{-# LANGUAGE DeriveGeneric #-}
module Marlowe.Symbolic.Types.Response where

import           Data.Aeson hiding (Result)
import           Data.ByteString
import           GHC.Generics

data APIGatewayResponse = APIGatewayResponse
  { statusCode :: Int
  , headers :: [(String, String)]
  , body :: String
  } deriving (Generic)
instance ToJSON APIGatewayResponse 

data Result = Valid
            | CounterExample
                { initialSlot :: Integer
                , transactionList :: String
                , transactionWarning :: String
                }
  deriving (Generic)
instance ToJSON Result

data Response = Response
  { uuid :: String
  , result :: Result 
  } deriving (Generic)
instance ToJSON Response

