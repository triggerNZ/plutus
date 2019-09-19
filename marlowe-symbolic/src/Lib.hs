module Lib where

import           Aws.Lambda
import           Control.Concurrent (forkOS, threadDelay, killThread)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, putMVar)
import           Control.Exception (try)
import           Data.Aeson (encode)
import           Data.ByteString.UTF8 as BSU
import Data.Proxy (Proxy(Proxy))
import           Language.Marlowe.Semantics (Slot(Slot), TransactionInput, TransactionWarning)
import           Language.Marlowe.Analysis.FSSemantics (warningsTrace)
import           Marlowe.Symbolic.Types.Request(Request(Request,contract,callbackUrl))
import qualified Marlowe.Symbolic.Types.Request as Req
import           Marlowe.Symbolic.Types.Response(Response(Response,result),Result(Valid, Error, CounterExample, initialSlot, transactionList, transactionWarning))
import           Marlowe.Symbolic.Types.API (API)
import qualified Marlowe.Symbolic.Types.Response as Res
import           Network.HTTP.Client (newManager)
import           System.Process (rawSystem)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (ClientEnv, ClientM, client, runClientM, parseBaseUrl, mkClientEnv)
import           Servant.API                    (NoContent)

notifyApi :: Proxy API
notifyApi = Proxy

notifyClient :: Response -> ClientM NoContent
notifyClient = client notifyApi

sendRequest :: String -> Response -> IO ()
sendRequest cu resp = do
  baseUrl <- parseBaseUrl cu
  manager <- newManager tlsManagerSettings
  let clientEnv = mkClientEnv manager baseUrl
  res <- runClientM (notifyClient resp) clientEnv
  print res

makeResponse :: String ->
                Either String (Maybe (Slot, [TransactionInput], [TransactionWarning]))
             -> Response
makeResponse u (Left err) = Response {Res.uuid = u, result = Error (show err)}
makeResponse u (Right res) =
   Response
     { Res.uuid = u
     , result = case res of
                  Nothing -> Valid
                  Just (Slot sn, ti, tw) ->
                     CounterExample
                       { initialSlot = sn
                       , transactionList = show ti
                       , transactionWarning = show tw 
                       }
     }

showIfLeft :: Show a => Either a b -> Either String b
showIfLeft (Left a) = Left (show a)
showIfLeft (Right x) = Right x

handler :: Request -> Context -> IO (Either Response Response)
handler Request {Req.uuid = u, callbackUrl = cu, contract = c} context =
  do rawSystem "killall" ["-q", "-9", "z3"]
     semaphore <- newEmptyMVar
     mainThread <-
       forkOS (do evRes <- (warningsTrace (read c))
                  (forkOS (do threadDelay 1000000 -- Timeout to send HTTP request (1 sec)
                              putMVar semaphore
                                (makeResponse u (Left "Response HTTP request timed out"))))
                  let resp = makeResponse u (showIfLeft evRes)
                  sendRequest cu resp
                  putMVar semaphore resp)
     timerThread <-
       forkOS (do threadDelay 110000000 -- Timeout in microseconds (1 min 50 sec)
                  putMVar semaphore (makeResponse u $ Left "Symbolic evaluation timed out"))
     x <- readMVar semaphore
     killThread mainThread
     killThread timerThread
     rawSystem "killall" ["-q", "-9", "z3"]
     return $ Right x


