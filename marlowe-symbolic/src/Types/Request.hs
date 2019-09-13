{-# LANGUAGE DeriveGeneric #-}
module Types.Request where

import           Data.Aeson
import           GHC.Generics

data Request = Request
  { uuid :: String
  , contract :: String
  } deriving (Generic)
instance FromJSON Request
instance ToJSON Request

