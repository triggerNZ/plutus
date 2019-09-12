{-# LANGUAGE DeriveGeneric #-}
module Lib where

import Aws.Lambda
import GHC.Generics
import Data.Aeson
import Language.Marlowe.Analysis.FSSemantics (warningsTrace)

data Request = Request
  { contract :: String
  } deriving (Generic)
instance FromJSON Request 
instance ToJSON Request

handler :: Request -> Context -> IO (Either String String)
handler (Request {contract = c}) context =
  do x <- warningsTrace (read c) 
     return (case x of
               Left err -> Left (show err)
               Right res -> Right (show res))

