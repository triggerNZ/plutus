{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Marlowe.Config where

import           Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Text (Text)

data Config = Config
    { _symbolicUrl :: Text 
    , _apiKey :: Text
    }

instance FromJSON Config where
    parseJSON =
        withObject "config" $ \o -> do
            _symbolicUrl <- o .: "symbolic-url"
            _apiKey <- o .: "api-key"
            pure Config {..}