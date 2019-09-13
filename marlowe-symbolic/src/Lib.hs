module Lib where

import           Aws.Lambda
import           Language.Marlowe.Semantics (Slot(Slot))
import           Language.Marlowe.Analysis.FSSemantics (warningsTrace)
import           Marlowe.Symbolic.Types.Error(Error(Error))
import qualified Marlowe.Symbolic.Types.Error as Err
import           Marlowe.Symbolic.Types.Request(Request(Request,contract))
import qualified Marlowe.Symbolic.Types.Request as Req
import           Marlowe.Symbolic.Types.Response(Response(Response,result),Result(..))
import qualified Marlowe.Symbolic.Types.Response as Res

handler :: Request -> Context -> IO (Either Error Response)
handler (Request {Req.uuid = u, contract = c}) context =
  do x <- warningsTrace (read c)
     return (case x of
               Left err -> Left (Error {Err.uuid = u, Err.error = (show err)})
               Right res ->
                 Right (Response
                          { Res.uuid = u
                          , result = case res of
                                       Nothing -> Valid
                                       Just (Slot sn, ti, tw) -> CounterExample
                                                 { initialSlot = sn
                                                 , transactionList = show ti
                                                 , transactionWarning = show tw 
                                                 }
                          }))

