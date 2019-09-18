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
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Map.Strict (Map)
import Data.UUID.V4 (nextRandom)
import Control.Newtype.Generics (Newtype, over, unpack)
import GHC.Generics (Generic)
import Control.Exception (handle, SomeException)
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM.TVar (TVar, newTVar)
import Control.Concurrent.STM (STM)
import qualified Marlowe.Symbolic.Types.Response as MSRes

newtype WebSocketRequestMessage
    = CheckForWarnings String
    deriving (Generic, ToJSON, FromJSON)

data WebSocketResponseMessage
    = CheckForWarningsResult MSRes.Result
    | OtherError String
    deriving (Generic, ToJSON, FromJSON)

-- | Each Connection is allowed only 1 active request at a time
--   We model this with Maybe since we also want the @UUID@ of this request
newtype Registry = Registry (Map UUID (Connection, Maybe UUID))
    deriving (Generic, Newtype)

newRegistry :: STM (TVar Registry)
newRegistry = newTVar $ Registry Map.empty

insertIntoRegistry :: UUID -> Connection -> Registry -> Registry
insertIntoRegistry uuid connection = over Registry (Map.insert uuid (connection, Nothing))

deleteFromRegistry :: UUID -> Registry -> Registry
deleteFromRegistry uuid = over Registry (Map.delete uuid)

lookupInRegistry :: UUID -> Registry -> Maybe (Connection, Maybe UUID)
lookupInRegistry uuid = Map.lookup uuid . unpack

startWaiting :: UUID -> UUID -> Registry -> Registry
startWaiting uuid waiting = over Registry (Map.adjust (\(connection, _) -> (connection, Just waiting)) uuid)

finishWaiting :: UUID -> Registry -> Registry
finishWaiting uuid = over Registry (Map.adjust (\(connection, _) -> (connection, Nothing)) uuid)

isWaiting :: UUID -> UUID -> Registry -> Bool
isWaiting uuid waiting registry = case Map.lookup uuid (unpack registry) of
                                    Nothing -> False
                                    Just (_, currentWaiting) -> Just waiting == currentWaiting

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
            f msg
            pure Nothing
    where
        disconnect :: SomeException -> IO ()
        disconnect _ = pure ()
