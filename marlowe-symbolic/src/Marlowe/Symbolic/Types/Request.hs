{-# LANGUAGE DeriveGeneric #-}
module Marlowe.Symbolic.Types.Request where

import           Data.Aeson
import           GHC.Generics

data APIGatewayRequest = APIGatewayRequest
  { resource :: String
  , body :: String
  } deriving (Generic)
instance FromJSON APIGatewayRequest

data Request = Request
  { uuid :: String
  , contract :: String
  } deriving (Generic)
instance FromJSON Request

