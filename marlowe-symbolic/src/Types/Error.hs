{-# LANGUAGE DeriveGeneric #-}
module Types.Error where

import           Data.Aeson
import           GHC.Generics

data Error = Error
  { uuid :: String
  , error :: String 
  } deriving (Generic)
instance FromJSON Error
instance ToJSON Error

