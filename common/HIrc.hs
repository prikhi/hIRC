{-# LANGUAGE DeriveGeneric #-}
module HIrc where

import Control.Concurrent.STM (TQueue)
import Control.Concurrent.STM.TMQueue (TMQueue)
import Data.Binary (Binary)
import GHC.Generics (Generic)

import qualified Data.Text as T

-- Messages & Queues

-- | A closeable queue for messages to a Client.
type ClientQueue
    = TMQueue ClientMsg

-- | Messages Daemons can send to a Client
-- TODO: NewServer, NewChannel, ServerMessage
data ClientMsg
    = Hello ClientId [(ServerName, ChannelName)]
    -- ^ Provide the client with an initial list of channels.
    | Subscriptions [((ServerName, ChannelName), [ChatMessage])]
    -- ^ Initial channel data returned after a `Subscribe` message.
    | NewMessage ServerName ChannelName ChatMessage
    -- ^ A new message has arrived in the channel.
    deriving (Generic, Show)

instance Binary ClientMsg


-- | A queue containing message from Clients to the Daemon
type DaemonQueue
    = TQueue (ClientId, DaemonMsg)

-- | Messages Clients can send to the Daemon
-- TODO: ConnectToServer, JoinChannel
data DaemonMsg
    = Subscribe [(ServerName, ChannelName)]
    -- ^ Notify the Client with updates to the given channels.
    | SendMessage ServerName ChannelName ChatMessage
    -- ^ Send a message to the specified channel.
    | Goodbye
    -- ^ Close the connection between the Client & Daemon.
    deriving (Generic, Show)

instance Binary DaemonMsg


-- Basic Types

newtype ClientId
    = ClientId
        { getClientId :: Integer
        } deriving (Show, Eq, Ord, Generic)

instance Binary ClientId

-- TODO Eventually add time & username
-- Split into ChannelMessage & ServerMessage?
newtype ChatMessage
    = Message
        { getMessage :: T.Text
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
