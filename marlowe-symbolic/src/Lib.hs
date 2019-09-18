module Lib where

import           Aws.Lambda
import           Control.Concurrent (forkOS, threadDelay, killThread)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, putMVar)
import           Control.Exception (try)
import           Data.Aeson (encode)
import           Data.ByteString.UTF8 as BSU
import           Language.Marlowe.Semantics (Slot(Slot), TransactionInput, TransactionWarning)
import           Language.Marlowe.Analysis.FSSemantics (warningsTrace)
import           Marlowe.Symbolic.Types.Request(Request(Request,contract,callbackUrl))
import qualified Marlowe.Symbolic.Types.Request as Req
import           Marlowe.Symbolic.Types.Response(Response(Response,result),Result(..))
import qualified Marlowe.Symbolic.Types.Response as Res
import           Network.HTTP.Client hiding (Request, Response)
import           System.Process (rawSystem)

sendRequest :: String -> Response -> IO ()
sendRequest cu resp =
  do manager <- newManager defaultManagerSettings
     maybeRequest <- try (parseRequest cu) 
     (case maybeRequest of
        Left (InvalidUrlException _ _) -> return ()
        Right request -> do httpLbs (request { method = BSU.fromString "POST"
                                             , requestBody = RequestBodyLBS $ encode $ resp })
                                    manager
                            return ())

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
handler (Request {Req.uuid = u, callbackUrl = cu, contract = c}) context =
  do rawSystem "killall" ["-9", "z3"]
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
     rawSystem "killall" ["-9", "z3"]
     return $ Right x


