{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Types
  ( Config(..)
  ) where

import qualified Auth
import qualified Marlowe.Config as MC
import           Data.Aeson (FromJSON, parseJSON, withObject, (.:))

data Config = Config
  { _authConfig :: Auth.Config
  , _marloweConfig :: MC.Config
  }

instance FromJSON Config where
  parseJSON =
    withObject "config" $ \o -> do
      _authConfig <- o .: "auth"
      _marloweConfig <- o .: "marlowe"
      pure Config {..}
