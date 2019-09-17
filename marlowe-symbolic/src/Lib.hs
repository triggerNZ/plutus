module Lib where

import           Data.ByteString.Lazy.UTF8 (fromString, toString)
import           Data.Maybe (listToMaybe)
import           Data.Aeson (encode, eitherDecode)
import           Data.List (find)
import           Aws.Lambda
import           Language.Marlowe.Semantics (Slot(Slot),Contract)
import           Language.Marlowe.Analysis.FSSemantics (warningsTrace)
import           Marlowe.Symbolic.Types.Error(Error(Error))
import qualified Marlowe.Symbolic.Types.Error as Err
import           Marlowe.Symbolic.Types.Request(Request(Request,contract),APIGatewayRequest(..))
import qualified Marlowe.Symbolic.Types.Request as Req
import           Marlowe.Symbolic.Types.Response(Response(Response,result),Result(..),APIGatewayResponse(..), Headers(..), getHeaders)
import qualified Marlowe.Symbolic.Types.Response as Res
import           Network.HTTP (simpleHTTP, postRequestWithBody, HStream)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

mkError :: String -> String -> Error
mkError u s = Error { Err.uuid = u
                    , Err.error = s }

analyseContract :: String -> Contract -> IO (Either String Response)
analyseContract u c =
  do x <- warningsTrace c
     return (case x of
               Left err -> Left $ show err
               Right res ->
                 Right (Response
                          { Res.uuid = u
                          , result = case res of
                                       Nothing -> Valid
                                       Just (Slot sn, ti, tw) ->
                                         CounterExample { initialSlot = sn
                                                        , transactionList = show ti
                                                        , transactionWarning = show tw 
                                                        }
                         }))

mkResponse :: Int -> String -> String -> APIGatewayResponse
mkResponse sc ct bodyStr =
  APIGatewayResponse { statusCode = sc
                     , headers = Headers [("Content-Type", ct)]
                     , Res.body = bodyStr }

mkWrongRequest :: String -> Maybe String -> APIGatewayResponse
mkWrongRequest err Nothing = mkResponse 400 "text/plain" err
mkWrongRequest err (Just u) =
  mkResponse 400 "application/json" $ toString $ encode $ mkError u err

mkInternalError :: String -> String -> APIGatewayResponse
mkInternalError err u =
  mkResponse 500 "application/json" $ toString $ encode $ mkError u err

handleRequest :: APIGatewayRequest -> Context -> IO (APIGatewayResponse, Maybe String)
handleRequest apiGatewayRequest context =
  (case eitherDecode (fromString (Req.body apiGatewayRequest)) of
     Right (Request {Req.uuid = u, contract = cs}) ->
       (case maybeRead cs of
          Just c -> do acr <- analyseContract u c
                       return ((case acr of
                                 Right resp ->  mkResponse 200 "application/json"
                                                  $ toString $ encode $ resp
                                 Left err -> mkInternalError err u), Just u)
          Nothing -> return $ (mkWrongRequest "Could not parse contract" (Just u), Just u))
     Left str -> return $ (mkWrongRequest ("Could not parse request body: \"" ++ str ++ "\"") Nothing, Nothing))

getContentType :: APIGatewayResponse -> String
getContentType agr = case find (\(x,_) -> x == "Content-Type") (getHeaders agr) of
                       Nothing -> "application/octet-stream"
                       Just (_, y) -> y

postResponse :: Maybe String -> APIGatewayResponse -> IO ()
postResponse Nothing _ = return ()
postResponse (Just u) agr =
  do simpleHTTP (postRequestWithBody u (getContentType agr) (Res.body agr))
     return ()

handler :: APIGatewayRequest -> Context -> IO (Either APIGatewayResponse APIGatewayResponse)
handler apiGatewayRequest context =
  do (resp, u) <- handleRequest apiGatewayRequest context
     postResponse u resp
     return $ Right resp

