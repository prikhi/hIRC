{-# LANGUAGE DeriveGeneric #-}
module HIrc where

import Control.Concurrent.STM (TQueue)
import Control.Concurrent.STM.TMQueue (TMQueue)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT)
import Data.Binary.Orphans (Binary)
import Data.Time (ZonedTime)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserDataDir, getUserDataFile)

import qualified Data.Text as T


-- Client

-- | A closeable queue for messages to a Client.
type ClientQueue
    = TMQueue ClientMsg

-- | Messages Daemons can send to a Client
-- TODO: NewServer, NewChannel, ServerMessage
data ClientMsg
    = Hello HelloData
    -- ^ Provide the client with an initial list of channels.
    | Subscriptions SubscriptionsData
    -- ^ Initial channel data returned after a `Subscribe` message.
    | NewMessage NewMessageData
    -- ^ A new message has arrived in the channel.
    deriving (Show, Generic)
instance Binary ClientMsg

data HelloData
    = HelloData
        { yourClientId :: ClientId
        -- ^ The ID the Client Should Use in `DaemonRequests`.
        , availableChannels :: [ChannelId]
        -- ^ The Channels the Client Can Subscribe To.
        } deriving (Show, Generic)
instance Binary HelloData

-- | TODO: Maybe just send a Map the Client can union with their Map.
newtype SubscriptionsData
    = SubscriptionsData
        { subscriptionLogs :: [(ChannelId, [ChatMessage])]
        -- ^ The Chat Logs for Each Newly Subscribed Channel
        } deriving (Show, Generic)
instance Binary SubscriptionsData

data NewMessageData
    = NewMessageData
        { newMessageTarget :: ChannelId
        , newMessage :: ChatMessage
        } deriving (Show, Generic)
instance Binary NewMessageData


-- Daemon

-- | A queue containing message from Clients to the Daemon
type DaemonQueue
    = TQueue DaemonRequest

-- | Messages Clients can send to the Daemon
data DaemonRequest
    = DaemonRequest
        { sourceClient :: ClientId
        -- ^ The ID of the Client generating the request.
        , daemonMsg :: DaemonMsg
        -- ^ The Message sent by the Client.
        } deriving (Show, Generic)
instance Binary DaemonRequest

data DaemonMsg
    = Subscribe SubscribeData
    -- ^ Subscribe the Client to the Requested Channels
    | SendMessage SendMessageData
    -- ^ Send a Message to a Specific Channel
    | Goodbye
    -- ^ Close the Connection between the Client & Daemon
    deriving (Show, Generic)
instance Binary DaemonMsg

newtype SubscribeData
    = SubscribeData
        { requestedChannels :: [ChannelId]
        -- ^ The Channels the Client Wants to Subscribe to.
        } deriving (Show, Generic)
instance Binary SubscribeData

data SendMessageData
    = SendMessageData
        { messageTarget :: ChannelId
        -- ^ The Channel for the Message.
        , messageContents :: T.Text
        -- ^ The Text of the Message.
        } deriving (Show, Generic)
instance Binary SendMessageData


-- Basic Types

-- | Channels are identified by their name & the server they're on.
data ChannelId
    = ChannelId ServerName ChannelName
    deriving (Show, Eq, Ord, Generic)
instance Binary ChannelId

newtype ClientId
    = ClientId
        { getClientId :: Integer
        } deriving (Show, Eq, Ord, Generic)

instance Binary ClientId

newtype UserName
    = UserName
        { getUserName :: T.Text
        } deriving (Show, Generic)
instance Binary UserName

-- TODO Will need some way to signify channel & server messages(e.g., topic
-- change).
data ChatMessage
    = Message
        { messageText :: T.Text
        , messageUser :: UserName
        , messageTime :: ZonedTime
        } deriving (Generic, Show)

instance Binary ChatMessage

newtype ServerName
    = ServerName
        { getServerName :: T.Text
        } deriving (Eq, Ord, Generic, Show)

instance Binary ServerName

newtype ChannelName
    = ChannelName
        { getChannelName :: T.Text
        } deriving (Eq, Ord, Generic, Show)

instance Binary ChannelName



-- Classes

class Monad m => GetSocketPath m where
    getSocketPath :: m FilePath

instance GetSocketPath IO where
    getSocketPath = do
        let dirName = "hirc"
        getUserDataDir dirName >>= createDirectoryIfMissing True
        getUserDataFile dirName "daemon.sock"

instance MonadIO m => GetSocketPath (ReaderT env m) where
    getSocketPath =
        liftIO getSocketPath
