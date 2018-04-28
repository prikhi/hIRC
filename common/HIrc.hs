{-# LANGUAGE DeriveGeneric #-}
module HIrc where

import Control.Concurrent.STM (TQueue)
import Control.Concurrent.STM.TMQueue (TMQueue)
import Data.Binary.Orphans (Binary)
import Data.Time (ZonedTime)
import GHC.Generics (Generic)

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
        , availableChannels :: [(ServerName, ChannelName)]
        -- ^ The Channels the Client Can Subscribe To.
        } deriving (Show, Generic)
instance Binary HelloData

-- | TODO: Maybe just send a Map the Client can union with their Map.
newtype SubscriptionsData
    = SubscriptionsData
        { subscriptionLogs :: [((ServerName, ChannelName), [ChatMessage])]
        -- ^ The Chat Logs for Each Newly Subscribed Channel
        } deriving (Show, Generic)
instance Binary SubscriptionsData

data NewMessageData
    = NewMessageData
        { newMessageTarget :: (ServerName, ChannelName)
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
        { requestedChannels :: [(ServerName, ChannelName)]
        -- ^ The Channels the Client Wants to Subscribe to.
        } deriving (Show, Generic)
instance Binary SubscribeData

data SendMessageData
    = SendMessageData
        { messageTarget :: (ServerName, ChannelName)
        -- ^ The Channel for the Message.
        , messageContents :: T.Text
        -- ^ The Text of the Message.
        } deriving (Show, Generic)
instance Binary SendMessageData


-- Basic Types

newtype ClientId
    = ClientId
        { getClientId :: Integer
        } deriving (Show, Eq, Ord, Generic)

instance Binary ClientId

-- TODO Eventually add UserNames
-- Split into ChannelMessage & ServerMessage
data ChatMessage
    = Message
        { messageText :: T.Text
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
