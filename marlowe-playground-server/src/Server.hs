{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC   -Wno-orphans     #-}

module Server
    ( mkHandlers
    )
where

import           API                           (API, RunResult, WSAPI)
import           Control.Monad                 (forever)
import           Control.Monad.Catch           (MonadCatch, MonadMask, bracket, catch)
import           Control.Monad.Except          (MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Logger          (MonadLogger, logInfoN)
import           Data.Aeson                    (ToJSON, encode)
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import Data.Maybe (fromMaybe)
import qualified Data.Text                     as Text
import           Data.Time.Units               (Microsecond, fromMicroseconds)
import qualified Data.UUID as UUID
import qualified Interpreter
import           Language.Haskell.Interpreter  (InterpreterError (CompilationErrors), InterpreterResult,
                                                SourceCode (SourceCode))
import           Marlowe.Contracts             (escrow)
import           Network.HTTP.Types            (hContentType)
import           Network.WebSockets.Connection (PendingConnection, receiveData, sendTextData)
import           Servant                       (ServantErr, err400, errBody, errHeaders)
import           Servant.API                   ((:<|>) ((:<|>)), (:>), JSON, Post, ReqBody)
import           Servant.Server                (Handler, Server)
import           System.Timeout                (timeout)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, readTVarIO)
import Control.Concurrent.STM (atomically)
import WebSocket (Registry, newRegistry, initializeConnection, runWithConnection, insertIntoRegistry, deleteFromRegistry, lookupInRegistry)
import qualified Marlowe.Symbolic.Types.Response   as MS
import qualified Marlowe.Symbolic.Types.API   as MS

acceptSourceCode :: SourceCode -> Handler (Either InterpreterError (InterpreterResult RunResult))
acceptSourceCode sourceCode = do
    let maxInterpretationTime :: Microsecond = fromMicroseconds (10 * 1000 * 1000)
    r <-
        liftIO
        $ runExceptT
        $ Interpreter.runHaskell maxInterpretationTime sourceCode
    case r of
        Right vs                        -> pure $ Right vs
        Left (CompilationErrors errors) -> pure . Left $ CompilationErrors errors
        Left  e                         -> throwError $ err400 { errBody = BSL.pack . show $ e }

checkHealth :: Handler ()
checkHealth = do
    res <- acceptSourceCode . SourceCode . Text.pack . BS.unpack $ escrow
    case res of
        Left e  -> throwError $ err400 {errBody = BSL.pack . show $ e}
        Right _ -> pure ()

handleWS :: TVar Registry -> PendingConnection -> Handler ()
handleWS registry pending = liftIO $ do
    (uuid, connection) <- initializeConnection pending
    atomically . modifyTVar registry $ insertIntoRegistry uuid connection
    runWithConnection connection f
    atomically . modifyTVar registry $ deleteFromRegistry uuid
    putStrLn "closed connection"
    where
        -- TODO: here we should actually send a message to the AWS API gateway with the request
        f :: Text.Text -> IO ()
        f = putStrLn . Text.unpack

handleNotification :: TVar Registry -> MS.Response -> Handler ()
handleNotification registry response = liftIO $ do
    registry <- readTVarIO registry
    let mConnection = do
            uuid <- UUID.fromString $ MS.uuid response
            lookupInRegistry uuid registry
    case mConnection of
        Nothing -> putStrLn "can't find user for response, they've probably disconnected"
        Just connection -> sendTextData connection $ encode response


{-# ANN mkHandlers
          ("HLint: ignore Avoid restricted function" :: String)
        #-}

mkHandlers :: (MonadLogger m, MonadIO m) => m (Server (API :<|> MS.API :<|> WSAPI))
mkHandlers = do
    logInfoN "Interpreter ready"
    registry <- liftIO $ atomically newRegistry
    pure $ (acceptSourceCode :<|> checkHealth) :<|> (handleNotification registry) :<|> (handleWS registry)
