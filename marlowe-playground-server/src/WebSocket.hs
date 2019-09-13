{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass     #-}
module WebSocket where

import Network.WebSockets (WebSocketsData)
import Network.WebSockets.Connection (Connection, PendingConnection, acceptRequest, forkPingThread, receiveData)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad (forever)
import Data.UUID (UUID)
import Data.Map.Strict (Map)
import Data.UUID.V4 (nextRandom)
import Control.Newtype.Generics (Newtype, over, unpack)
import GHC.Generics (Generic)
import Control.Exception (handle, SomeException)
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM.TVar (TVar, newTVar)
import Control.Concurrent.STM (STM)

newtype Registry = Registry (Map UUID Connection)
    deriving (Generic, Newtype)

newRegistry :: STM (TVar Registry)
newRegistry = newTVar $ Registry Map.empty

insertIntoRegistry :: UUID -> Connection -> Registry -> Registry
insertIntoRegistry uuid connection = over Registry (Map.insert uuid connection)

deleteFromRegistry :: UUID -> Registry -> Registry
deleteFromRegistry uuid = over Registry (Map.delete uuid)

lookupInRegistry :: UUID -> Registry -> Maybe Connection
lookupInRegistry uuid = Map.lookup uuid . unpack

-- | Take a @PendingConnection@ and returns a @UUID@ and @Connection$ for the user
initializeConnection :: PendingConnection -> IO (UUID, Connection)
initializeConnection pending = do
    connection <- acceptRequest pending
    forkPingThread connection 30
    uuid <- nextRandom
    pure (uuid, connection)

-- | Run an IO function that keeps being applied to new messages being recieved
--   This function terminates when the connection is closed
runWithConnection :: (WebSocketsData a) => Connection -> (a -> IO ()) -> IO ()
runWithConnection connection f = handle disconnect . forever $ do
            msg <- receiveData connection
            result <- f msg
            pure Nothing
    where
        disconnect :: SomeException -> IO ()
        disconnect _ = pure ()
