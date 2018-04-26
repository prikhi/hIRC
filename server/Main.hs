{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.Async.Lifted (Async, Concurrently(..), async, cancel)
import Control.Concurrent.STM
    ( TVar, TQueue, atomically, newTVarIO, modifyTVar, newTQueueIO, writeTQueue
    , readTQueue, orElse, readTVarIO
    )
import Control.Exception.Safe (bracket)
import Control.Lens ((&), (.~), (%~))
import Control.Monad (void, forever)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, runReaderT, asks, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl, StM)
import Data.Binary (Binary)
import Data.Maybe (fromMaybe)
import Data.Conduit ((.|), ConduitT, Void, runConduit)
import Data.Conduit.Serialization.Binary (conduitEncode, conduitDecode)
import Data.Conduit.Network.Unix (AppDataUnix, serverSettings, runUnixServer, appSource, appSink)
import Data.Conduit.TQueue (sinkTQueue, sourceTQueue)
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Network.IRC.Client
    ( IRC, IRCState, Event(..), EventHandler(..), Source(..), Message(..)
    , ConnectionConfig, InstanceConfig, plainConnection, defaultInstanceConfig
    , runClientWith, logfunc, stdoutLogger, channels, nick, password, send
    , onconnect, handlers, newIRCState, runIRCAction
    )

import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text as T

-- TODO: Kill irc connections on exception
main :: IO ()
main = do
    env <- initialize
    flip runReaderT env $ do
        connectToServers
        bracket runSocketServer (liftIO . cancel) (const handleQueues)


-- TODO: handle per-client queues
-- Clients should subscribe to certain channels as their initial message.
-- This should add them to a map like:
--      Map (ServerName, ChannelName) [ClientId]
-- We should also keep a map of client queues:
--      Map ClientId ClientQueue
-- When a channel gets a message, we will have the list of clients to
-- update, and the queues to broadcast the message to.
--
-- There may also be a better way using TChans & cloning instead of
-- TQueues, maybe explore that afterwards.


-- | Create the Message Queues & the Initial IRC Server Data.
initialize :: IO Env
initialize = do
    let config = testConfig
    (cQueue, dQueue, sQueue) <- (,,) <$> newTQueueIO <*> newTQueueIO <*> newTQueueIO
    serverData <- sequence $ M.map (const $ newTVarIO emptyServerData) $ serverConfig config
    return $ Env config serverData cQueue dQueue sQueue
    where
        emptyServerData =
            ServerData M.empty [] Nothing

testConfig :: Config
testConfig =
    Config (UserName "test-user") $ M.fromList
        [ ( ServerName "TestServer"
          , ServerConfig
            { userName = Nothing
            , serverPassword = Just $ Password "test"
            , serverHost = ServerHost "localhost"
            , serverPort = ServerPort 6667
            , security = Plain
            , defaultChannels = map ChannelName ["#test-channel", "#other-channel"]
            }
          )
        ]


-- | Connect to all the IRC Servers Specified in the `Config`.
-- TODO: restrict typeclass - specify that we can only add things to the queue
connectToServers
    :: ( HasDefaultUserName env
       , HasServerConfigs env
       , HasIrcQueue env
       , MonadReader env m
       , IrcConnection m
       )
    => m ()
connectToServers = do
    serverConfigs <- asks getServerConfigs
    name <- asks getDefaultUser
    void . sequence $ M.mapWithKey (connectToServer name) serverConfigs

-- | Connect to an IRC Server.
connectToServer
    :: (HasIrcQueue env, MonadReader env m, IrcConnection m)
    => UserName
    -> ServerName
    -> ServerConfig
    -> m ()
connectToServer defaultName serverName config = do
    ircQueue <- asks getIrcQueue
    let name = fromMaybe defaultName $ userName config
        host = serverHost config
        port = serverPort config
        conn = plainConnection (getServerHost host) (getServerPort port)
            & logfunc .~ stdoutLogger
            & password .~ (getPassword <$> serverPassword config)
            & onconnect %~ (>> identify name)
        conf = defaultInstanceConfig (getUserName name)
            & nick .~ getUserName name
            & channels .~ map getChannelName (defaultChannels config)
            & handlers %~ (receiveMessageHandler ircQueue :)
    connectToIrcServer serverName conn conf
    where
        -- TODO: ServerConfig should specify these commands because they are dependent on the IRC server
        identify :: UserName -> IRC s ()
        identify name =
            case serverPassword config of
                Just (Password pass) -> do
                    send . RawMsg
                        $ "ACC REGISTER " <> getUserName name <> " * passphrase :" <> pass
                    send . Privmsg "NickServ" . Right
                        $ "IDENTIFY " <> getUserName name <> " " <> pass
                Nothing ->
                    return ()
        -- TODO: Eventually the `s` state should change to `ClientId`?
        receiveMessageHandler :: IrcQueue -> EventHandler s
        receiveMessageHandler ircQueue =
            EventHandler matcher handler
            where
                matcher :: Event T.Text -> Maybe IrcMsg
                matcher ev =
                    case (_message ev, _source ev) of
                        (Privmsg _ (Right msg), Channel name senderNick) ->
                            Just $ ReceiveMessage
                                serverName
                                (ChannelName name)
                                (UserName senderNick)
                                (Message msg)
                        _ ->
                            Nothing
                handler :: Source T.Text -> IrcMsg -> IRC s ()
                handler _ msg =
                    liftIO $ atomically $ writeTQueue ircQueue msg

-- | Run the unix socket server, piping together the queues & socket
-- clients.
runSocketServer :: (MonadBaseControl IO m, RunSocketServer m) => m (Async (StM m ()))
runSocketServer =
    async $ runDaemonServer "hircd.sock" handleConnection
    where
        -- TODO: Send (server, channel) list before tqueue sink/sources
        handleConnection :: ClientQueue -> DaemonQueue -> AppDataUnix -> IO ()
        handleConnection clientQueue daemonQueue connection =
            asyncConduits
                [ sourceTQueue clientQueue .| conduitEncode .| appSink connection
                , appSource connection .| conduitDecode .| sinkTQueue daemonQueue
                ]
        asyncConduits :: [ConduitT () Void IO ()] -> IO ()
        asyncConduits =
            runConcurrently .
                foldl (\acc i -> acc *> Concurrently (runConduit i))
                    (Concurrently $ pure ())

-- | Message types handled by the Daemon.
data QueueMsg
    = DaemonMsg DaemonMsg
    | IrcMsg IrcMsg

-- | Handle any incoming DaemonMsgs or IrcMsgs.
handleQueues
    :: (UpdateChannelLog m, SendClientMessage m, ReadQueues m, SendIrcMessage m)
    => m ()
handleQueues = forever $
    readQueueMessage >>= \case
        DaemonMsg msg ->
            handleDaemonMessage msg
        IrcMsg msg ->
            handleIrcMessage msg

handleDaemonMessage :: SendIrcMessage m => DaemonMsg -> m ()
handleDaemonMessage = \case
    -- Send the message to the IRC server
    SendMessage serverName channelName message ->
        sendIrcMessage serverName
            (Privmsg (getChannelName channelName) $ Right (getMessage message))

handleIrcMessage :: (UpdateChannelLog m, SendClientMessage m) => IrcMsg -> m ()
handleIrcMessage = \case
    -- Store the message & send it to the appropriate clients
    ReceiveMessage serverName channelName user message -> do
        updateChannelLog serverName channelName user message
        sendClientMessage $
            NewMessage serverName channelName message



-- Functionality Typeclasses

-- | Runs the Server for Clients
-- TODO: This'll need some sort of ClientId to differentiate which Client Queue.
class Monad m => RunSocketServer m where
    -- | Run a unix socket server at the given path. Write ClientMsgs
    -- to the socket & DaemonMsgs from the socket to the daemon queue.
    -- TODO: This type sig might be too coupled by using the TQueues
    runDaemonServer
        :: FilePath
        -> (ClientQueue -> DaemonQueue -> AppDataUnix -> IO ())
        -> m ()

instance (HasClientQueue env, HasDaemonQueue env, MonadIO m) => RunSocketServer (ReaderT env m) where
    -- | Communicate using TQueues.
    runDaemonServer socketPath handler = do
        clientQueue <- asks getClientQueue
        daemonQueue <- asks getDaemonQueue
        liftIO $ runUnixServer (serverSettings socketPath) (handler clientQueue daemonQueue)


-- | IRC Connection Manipulation
class Monad m => IrcConnection m where
    -- | Connect to an IRC server with the given Name & Configuration.
    connectToIrcServer :: ServerName -> ConnectionConfig () -> InstanceConfig () -> m ()

instance (HasServerData env, MonadIO m) => IrcConnection (ReaderT env m) where
    -- | Run the IRC client in a separate thread & save the Async value to
    -- the Server's `ServerData`.
    connectToIrcServer name conn conf =
        asks (getServerData name) >>= \case
            Nothing ->
                return ()
            Just serverData -> do
                ircState <- newIRCState conn conf ()
                connectionThread <- liftIO . async $ runClientWith ircState
                liftIO . atomically . modifyTVar serverData $
                    \d -> d { serverThread = Just (connectionThread, ircState) }


-- | Take Values from the Server-relevant Queues.
class Monad m => ReadQueues m where
    -- | Fetch the next `QueueMsg` to process, blocking until one is
    -- available.
    readQueueMessage :: m QueueMsg

instance (HasDaemonQueue env, HasIrcQueue env, MonadIO m) => ReadQueues (ReaderT env m) where
    -- | Pull from the Daemon & Irc Queues.
    readQueueMessage = do
        daemonQueue <- asks getDaemonQueue
        ircQueue <- asks getIrcQueue
        liftIO . atomically
            $ (DaemonMsg <$> readTQueue daemonQueue)
            `orElse` (IrcMsg <$> readTQueue ircQueue)


-- | Manipulate the Message Log for a Channel
class Monad m => UpdateChannelLog m where
    -- | Add a message to the log.
    updateChannelLog :: ServerName -> ChannelName -> UserName -> ChatMessage -> m ()

instance (HasServerData env, MonadIO m) => UpdateChannelLog (ReaderT env m) where
    updateChannelLog serverName channelName _ message =
        asks (getServerData serverName) >>= \case
            Nothing ->
                return ()
            Just dataTVar ->
                liftIO . atomically $ modifyTVar dataTVar $
                    \d -> d { channelMap = updateChannelMap $ channelMap d }
        where
            updateChannelMap =
                M.update
                    (\cData ->
                        Just $ cData { messageLog = message : messageLog cData }
                    )
                    channelName


-- | Send Messages to Clients
class Monad m => SendClientMessage m where
    sendClientMessage :: ClientMsg -> m ()

instance (HasClientQueue env, MonadIO m) => SendClientMessage (ReaderT env m) where
    sendClientMessage msg = do
        clientQueue <- asks getClientQueue
        liftIO . atomically $ writeTQueue clientQueue msg


-- | Send Messages to IRC Servers
class Monad m => SendIrcMessage m where
    sendIrcMessage :: ServerName -> Message T.Text -> m ()

instance (HasServerData env, MonadIO m) => SendIrcMessage (ReaderT env m) where
    sendIrcMessage serverName msg =
        asks (getServerData serverName) >>= \case
            Nothing ->
                return ()
            Just dataTVar -> do
                liftIO (serverThread <$> readTVarIO dataTVar) >>= \case
                    Nothing ->
                        return ()
                    Just (_, ircState) ->
                        runIRCAction (send msg) ircState
                return ()


-- Env Typeclasses

class HasDefaultUserName a where
    getDefaultUser :: a -> UserName

instance HasDefaultUserName Env where
    getDefaultUser = defaultUserName . envConfig


class HasServerConfigs a where
    getServerConfigs :: a -> M.Map ServerName ServerConfig

instance HasServerConfigs Env where
    getServerConfigs = serverConfig . envConfig


-- | Try to find a `ServerData` reference from a `ServerName`.
class HasServerData a where
    getServerData :: ServerName -> a -> Maybe (TVar ServerData)

instance HasServerData Env where
    getServerData name =
        M.lookup name . envServers


class HasClientQueue a where
    getClientQueue :: a -> ClientQueue

instance HasClientQueue Env where
    getClientQueue = envClientQueue


class HasDaemonQueue a where
    getDaemonQueue :: a -> DaemonQueue

instance HasDaemonQueue Env where
    getDaemonQueue = envDaemonQueue


class HasIrcQueue a where
    getIrcQueue :: a -> IrcQueue

instance HasIrcQueue Env where
    getIrcQueue = envIrcQueue





-- TYPES

type ServerM a
    = ReaderT Env IO a

data Env
    = Env
        { envConfig :: Config
        -- ^ The Static Application Configuration.
        , envServers :: M.Map ServerName (TVar ServerData)
        -- ^ Modifiable Data for Each Server
        , envClientQueue :: ClientQueue
        -- ^ A Queue Containing Messages for the Client from the Daemon
        , envDaemonQueue :: DaemonQueue
        -- ^ A Queue Containing Messages for the Daemon from the Client
        , envIrcQueue :: IrcQueue
        -- ^ A Queue Containing Messages for the Daemon from an IRC Server
        }


type ClientQueue
    = TQueue ClientMsg

data ClientMsg
    = NewMessage ServerName ChannelName ChatMessage
    deriving (Generic)

instance Binary ClientMsg


type DaemonQueue
    = TQueue DaemonMsg

data DaemonMsg
    = SendMessage ServerName ChannelName ChatMessage
    deriving (Generic)

instance Binary DaemonMsg


type IrcQueue
    = TQueue IrcMsg

data IrcMsg
    = ReceiveMessage ServerName ChannelName UserName ChatMessage
    deriving (Generic)

instance Binary IrcMsg

data Config
    = Config
        { defaultUserName :: UserName
        , serverConfig :: M.Map ServerName ServerConfig
        }

data ServerConfig
    = ServerConfig
        { userName :: Maybe UserName
        , serverPassword :: Maybe Password
        , serverHost :: ServerHost
        , serverPort :: ServerPort
        , security :: ServerSecurity
        , defaultChannels :: [ChannelName]
        }

data ServerData
    = ServerData
        { channelMap :: M.Map ChannelName ChannelData
        , serverLog :: [ChatMessage]
        , serverThread :: Maybe (Async (), IRCState ())
        }

data ChannelData
    = ChannelData
        { userList :: [UserName]
        , messageLog :: [ChatMessage]
        }

data ServerSecurity
    = Plain
    | TLS

-- Newtype Wrappers

newtype UserName
    = UserName
        { getUserName :: T.Text
        } deriving (Generic)

instance Binary UserName

newtype Password
    = Password
        { getPassword :: T.Text
        }

newtype ChatMessage
    = Message
        { getMessage :: T.Text
        } deriving (Generic)

instance Binary ChatMessage

newtype ServerName
    = ServerName
        { getServerName :: T.Text
        } deriving (Eq, Ord, Generic)

instance Binary ServerName

newtype ChannelName
    = ChannelName
        { getChannelName :: T.Text
        } deriving (Eq, Ord, Generic)

instance Binary ChannelName

newtype ServerHost
    = ServerHost
        { getServerHost :: B.ByteString
        }

newtype ServerPort
    = ServerPort { getServerPort :: Int
        }
