{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Concurrent.Async.Lifted (Async, Concurrently(..), async, cancel)
import Control.Concurrent.STM
    ( TVar, TQueue, atomically, newTVarIO, modifyTVar, newTQueueIO, writeTQueue
    , readTQueue, orElse, readTVarIO, readTVar, writeTVar
    )
import Control.Concurrent.STM.TMQueue (newTMQueue, writeTMQueue, closeTMQueue)
import Control.Exception.Safe (bracket)
import Control.Lens ((&), (.~), (%~), (^.))
import Control.Monad (void, forever, forM_)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, runReaderT, asks, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl, StM)
import Data.Aeson ((.:), (.:?), FromJSON(..),withObject, withText, withScientific)
import Data.Conduit ((.|), ConduitT, Void, runConduit)
import Data.Conduit.Network.Unix (AppDataUnix, serverSettings, runUnixServer, appSource, appSink)
import Data.Conduit.Serialization.Binary (conduitEncode, conduitDecode)
import Data.Conduit.TQueue (sinkTQueue, sourceTMQueue)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (ZonedTime, getZonedTime)
import Data.Yaml.Config (loadYamlSettings, ignoreEnv)
import Network.IRC.Client
    ( IRC, IRCState, Event(..), EventHandler(..), Source(..), Message(..)
    , ConnectionConfig, InstanceConfig, plainConnection, defaultInstanceConfig
    , runClientWith, channels, nick, password, send, onconnect, handlers
    , newIRCState, runIRCAction, instanceConfig, tlsConnection, TLSConfig(..)
    , username, realname
    )
import System.Environment (getArgs)
import System.Environment.XDG.BaseDir (getUserConfigFile)

import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text as T

import HIrc


-- TODO: Kill irc connections on exception
main :: IO ()
main = do
    env <- initialize
    flip runReaderT env $ do
        connectToServers
        bracket runSocketServer (liftIO . cancel) (const handleQueues)


-- TODO: It might be better to use TChans & cloning instead of TMQueues for
-- client communication. That way we could easily broadcast Server
-- & Channel messages to all subscribed clients.

-- | Create the Message Queues & the Initial IRC Server Data.
-- TODO: Refactor config parsing into function, handle exceptions w/ default config
initialize :: IO Env
initialize = do
    defaultConfigPath <- getUserConfigFile "hirc" "config.yaml"
    configPath <- fromMaybe defaultConfigPath . listToMaybe <$> getArgs
    config <- loadYamlSettings [configPath] [] ignoreEnv
    (cQueues, dQueue, iQueue) <- (,,) <$> newTVarIO M.empty <*> newTQueueIO <*> newTQueueIO
    serverData <- newTVarIO M.empty
    nextClient <- newTVarIO (ClientId 1)
    subscriptions <- newTVarIO M.empty
    return Env
        { envConfig = config
        , envServers = serverData
        , envClientQueues = cQueues
        , envDaemonQueue = dQueue
        , envIrcQueue = iQueue
        , envNextClientId = nextClient
        , envSubscriptions = subscriptions
        }


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
        baseConfig =
            case security config of
                Plain ->
                    plainConnection
                TLS ->
                    \h p -> tlsConnection (WithDefaultConfig h p)
        conn = baseConfig (getServerHost host) (getServerPort port)
            & username .~ getUserName name
            & realname .~ getUserName name
            & password .~ (getPassword <$> serverPassword config)
            & onconnect %~ (>> sendCommands (serverCommands config))
        conf = defaultInstanceConfig (getUserName name)
            & nick .~ getUserName name
            & channels .~ map getChannelName (defaultChannels config)
            & handlers %~ (receiveMessageHandler ircQueue :)
    connectToIrcServer serverName conn conf
    where
        sendCommands :: [Command] -> IRC s ()
        sendCommands =
            mapM_ (send . RawMsg . getCommand)
        -- TODO: Expand to handle more Message types
        -- Maybe pass all events to queue & filter/transform in main
        -- thread.
        receiveMessageHandler :: IrcQueue -> EventHandler s
        receiveMessageHandler ircQueue =
            EventHandler matcher handler
            where
                matcher :: Event T.Text -> Maybe (ZonedTime -> IrcMsg)
                matcher ev =
                    case (_message ev, _source ev) of
                        (Privmsg _ (Right msg), Channel name senderNick) ->
                            Just $ ReceivedMessage
                                (ChannelId serverName (ChannelName name))
                                . ChatMessage msg (UserName senderNick)
                        _ ->
                            Nothing
                handler :: Source T.Text -> (ZonedTime -> IrcMsg) -> IRC s ()
                handler _ msg =
                    liftIO
                        $ getZonedTime
                        >>= atomically . writeTQueue ircQueue . msg

-- | Run the unix socket server, piping together the queues & socket
-- clients.
runSocketServer
    :: ( HasServerMap env
       , HasNextClientId env
       , HasClientQueues env
       , MonadReader env m
       , MonadBaseControl IO m
       , RunSocketServer m
       , GetSocketPath m
       )
    => m (Async (StM m ()))
runSocketServer = do
    serverMap <- asks getServerMap
    clientId <- asks getNextClientId
    clientQueues <- asks getClientQueues
    socketPath <- getSocketPath
    async $ runDaemonServer socketPath
        (handleConnection serverMap clientId clientQueues)
    where
        handleConnection
            :: TVar (M.Map ServerName ServerData)
            -> TVar ClientId
            -> TVar (M.Map ClientId ClientQueue)
            -> DaemonQueue
            -> AppDataUnix
            -> IO ()
        handleConnection dataMapRef idRef clientQueues daemonQueue connection = do
            chanList <- makeChanList dataMapRef
            clientId <- runReaderT nextClientId idRef
            let helloMessage = Hello HelloData
                    { yourClientId = clientId, availableChannels = chanList }
            clientQueue <- atomically $ do
                queue <- newTMQueue
                modifyTVar clientQueues $ M.insert clientId queue
                writeTMQueue queue helloMessage
                return queue
            asyncConduits
                [ sourceTMQueue clientQueue .| conduitEncode .| appSink connection
                , appSource connection .| conduitDecode .| sinkTQueue daemonQueue
                ]
        makeChanList :: TVar (M.Map ServerName ServerData) -> IO [ChannelId]
        makeChanList dataRef = do
            serverMap <- M.toList <$> readTVarIO dataRef
            return . concat . flip map serverMap $ \(serverName, sData) ->
                map (ChannelId serverName) . M.keys $ channelMap sData
        asyncConduits :: [ConduitT () Void IO ()] -> IO ()
        asyncConduits =
            runConcurrently .
                foldl (\acc i -> acc *> Concurrently (runConduit i))
                    (Concurrently $ pure ())

-- | Join the message types handled by the Daemon.
data QueueMsg
    = DaemonQueueMsg DaemonRequest
    | IrcQueueMsg IrcMsg
    deriving (Show)

-- | Handle any incoming DaemonMsgs or IrcMsgs.
handleQueues
    :: ( UpdateChannelLog m
       , SendClientMessage m
       , ReadQueues m
       , SendIrcMessage m
       , GetServerData m
       , CloseClientQueue m
       , GetSubscribers m
       , AddSubscription m
       , GetTime m
       , GetUserName m
       )
    => m ()
handleQueues = forever $
    readQueueMessage >>= \case
        DaemonQueueMsg msg ->
            handleDaemonMessage (sourceClient msg) (daemonMsg msg)
        IrcQueueMsg msg ->
            handleIrcMessage msg

handleDaemonMessage
    :: ( GetServerData m
       , SendIrcMessage m
       , SendClientMessage m
       , CloseClientQueue m
       , GetSubscribers m
       , AddSubscription m
       , UpdateChannelLog m
       , GetTime m
       , GetUserName m
       )
    => ClientId
    -> DaemonMsg
    -> m ()
handleDaemonMessage clientId = \case
    -- Subscribe the client to the passed channels & reply with
    -- a Subscriptions message.
    Subscribe SubscribeData { requestedChannels } -> do
        mapM_ (subscribe clientId) requestedChannels
        subscriptions <- M.fromList . catMaybes <$>
            mapM (\c -> fmap (c,) <$> getChannelData c) requestedChannels
        sendClientMessage clientId $ Subscriptions $ SubscriptionsData subscriptions
        where
            getChannelData
                :: (GetServerData m) => ChannelId -> m (Maybe ChannelData)
            getChannelData (ChannelId serverName channelName) =
                getServerData serverName >>= \case
                    Nothing ->
                        return Nothing
                    Just sData ->
                        return $ M.lookup channelName (channelMap sData)
    -- Send the message to the IRC server & subscribed clients.
    SendMessage SendMessageData { messageTarget, messageContents } -> do
        let (ChannelId serverName channelName) = messageTarget
        sendIrcMessage serverName
            (Privmsg (getChannelName channelName) $ Right messageContents)
        time <- getTime
        myNick <- getCurrentUserName serverName
        let message = ChatMessage messageContents myNick time
        updateChannelLog messageTarget message
        clients <- getSubscribers messageTarget
        forM_ clients $ \subscriberId ->
            sendClientMessage subscriberId $
                NewMessage NewMessageData
                    { newMessageTarget = messageTarget
                    , newMessage = message
                    }
    -- Close the Client's Queue.
    Goodbye ->
        closeClientQueue clientId

handleIrcMessage
    :: (UpdateChannelLog m, SendClientMessage m, GetSubscribers m)
    => IrcMsg
    -> m ()
handleIrcMessage = \case
    -- Store the message & send it to the appropriate clients
    ReceivedMessage target message -> do
        updateChannelLog target message
        clients <- getSubscribers target
        forM_ clients $ \clientId ->
            sendClientMessage clientId $ NewMessage NewMessageData
                { newMessageTarget = target
                , newMessage = message
                }



-- Functionality Typeclasses

-- | Runs the Server for Clients
class Monad m => RunSocketServer m where
    -- | Run a unix socket server at the given path. Write ClientMsgs
    -- to the socket & DaemonMsgs from the socket to the daemon queue.
    -- TODO: This type sig might be too coupled by using the TQueue
    runDaemonServer
        :: FilePath
        -> (DaemonQueue -> AppDataUnix -> IO ())
        -> m ()

instance (HasDaemonQueue env, MonadIO m) => RunSocketServer (ReaderT env m) where
    -- | Communicate using TQueues.
    runDaemonServer socketPath handler = do
        daemonQueue <- asks getDaemonQueue
        liftIO $ runUnixServer (serverSettings socketPath) (handler daemonQueue)


-- | IRC Connection Manipulation
class Monad m => IrcConnection m where
    -- | Connect to an IRC server with the given Name & Configuration.
    connectToIrcServer :: ServerName -> ConnectionConfig () -> InstanceConfig () -> m ()

instance (HasServerMap env, MonadIO m) => IrcConnection (ReaderT env m) where
    -- | Run the IRC client in a separate thread & save the Async value to
    -- the Server's `ServerData`.
    -- TODO: forever + bracket, print exceptions
    connectToIrcServer name conn conf = do
        ircState <- newIRCState conn conf ()
        connectionThread <- liftIO . async $ runClientWith ircState
        updateServerData name $ \case
            Nothing ->
                Just ServerData
                    { channelMap = foldl
                        (\acc c -> M.insert (ChannelName c) (ChannelData [] []) acc)
                        M.empty (conf ^. channels)
                    , serverLog = []
                    , serverThread = Just (connectionThread, ircState)
                    }
            Just d ->
                Just d { serverThread = Just (connectionThread, ircState) }


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
            $ (DaemonQueueMsg <$> readTQueue daemonQueue)
            `orElse` (IrcQueueMsg <$> readTQueue ircQueue)


-- | Manipulate the Message Log for a Channel
class Monad m => UpdateChannelLog m where
    -- | Add a message to the log.
    updateChannelLog :: ChannelId -> ChannelMessage -> m ()

instance (HasServerMap env, MonadIO m) => UpdateChannelLog (ReaderT env m) where
    updateChannelLog (ChannelId serverName channelName) message =
        updateServerData serverName
            $ fmap (\d -> d { channelMap = updateChannelMap $ channelMap d })
        where
            updateChannelMap =
                M.update
                    (\cData ->
                        Just $ cData { messageLog = message : messageLog cData }
                    )
                    channelName


-- | Send Messages to Clients
class Monad m => SendClientMessage m where
    sendClientMessage :: ClientId -> ClientMsg -> m ()

instance (HasClientQueues env, MonadIO m) => SendClientMessage (ReaderT env m) where
    sendClientMessage cId msg = do
        maybeQueue <- getClientQueue cId
        case maybeQueue of
            Nothing ->
                return ()
            Just queue ->
                liftIO . atomically $ writeTMQueue queue msg


-- | Send Messages to IRC Servers
class Monad m => SendIrcMessage m where
    sendIrcMessage :: ServerName -> Message T.Text -> m ()

instance (HasServerMap env, MonadIO m) => SendIrcMessage (ReaderT env m) where
    sendIrcMessage serverName msg =
        getServerData serverName >>= \case
            Nothing ->
                return ()
            Just sData -> do
                case serverThread sData of
                    Nothing ->
                        return ()
                    Just (_, ircState) ->
                        liftIO $ runIRCAction (send msg) ircState
                return ()


class Monad m => GetServerData m where
    getServerData :: ServerName -> m (Maybe ServerData)

instance (HasServerMap env, MonadIO m) => GetServerData (ReaderT env m) where
    getServerData serverName =
        M.lookup serverName <$> (liftIO . readTVarIO =<< asks getServerMap)


class Monad m => UpdateServerData m where
    updateServerData :: ServerName -> (Maybe ServerData -> Maybe ServerData) -> m ()

instance (HasServerMap env, MonadIO m) => UpdateServerData (ReaderT env m) where
    updateServerData serverName alterFunc = do
        serverMap <- asks getServerMap
        liftIO $ atomically $ modifyTVar serverMap $ M.alter alterFunc serverName


class Monad m => GetClientQueue m where
    getClientQueue :: ClientId -> m (Maybe ClientQueue)

instance (MonadIO m, HasClientQueues env) => GetClientQueue (ReaderT env m) where
    getClientQueue clientId = do
        queuesRef <- asks getClientQueues
        M.lookup clientId <$> liftIO (readTVarIO queuesRef)


class Monad m => CloseClientQueue m where
    closeClientQueue :: ClientId -> m ()

instance (MonadIO m, HasClientQueues env) => CloseClientQueue (ReaderT env m) where
    closeClientQueue clientId = do
        queuesRef <- asks getClientQueues
        maybeQueue <- getClientQueue clientId
        case maybeQueue of
            Nothing ->
                return ()
            Just queue ->
                liftIO $ atomically $ do
                    closeTMQueue queue
                    modifyTVar queuesRef $ M.delete clientId


class Monad m => NextClientId m where
    nextClientId :: m ClientId

instance (HasNextClientId env, MonadIO m) => NextClientId (ReaderT env m) where
    nextClientId = do
        tvar <- asks getNextClientId
        liftIO . atomically $ do
            nextId <- readTVar tvar
            writeTVar tvar $ ClientId $ getClientId nextId + 1
            return nextId


class Monad m => AddSubscription m where
    subscribe :: ClientId -> ChannelId ->m ()

instance (HasSubscriptions env, MonadIO m) => AddSubscription (ReaderT env m) where
    subscribe clientId k = do
        tvar <- asks getSubscriptions
        liftIO . atomically . modifyTVar tvar $
            M.alter (Just . maybe [clientId] (clientId :)) k

class Monad m => GetSubscribers m where
    getSubscribers :: ChannelId -> m [ClientId]

instance (HasSubscriptions env, MonadIO m) => GetSubscribers (ReaderT env m) where
    getSubscribers k = do
        tvar <- asks getSubscriptions
        fromMaybe [] . M.lookup k <$> liftIO (readTVarIO tvar)


class Monad m => GetTime m where
    getTime :: m ZonedTime

instance MonadIO m => GetTime (ReaderT env m) where
    getTime = liftIO getZonedTime


class Monad m => GetUserName m where
    getCurrentUserName :: ServerName -> m UserName

instance (HasServerMap env, HasDefaultUserName env, MonadIO m) => GetUserName (ReaderT env m) where
    getCurrentUserName serverName =
        getServerData serverName >>= \case
            Nothing ->
                asks getDefaultUser
            Just serverData -> do
                let mState = snd <$> serverThread serverData
                case mState of
                    Nothing ->
                        asks getDefaultUser
                    Just ircState -> do
                        config <- liftIO $ readTVarIO $ ircState ^. instanceConfig
                        return . UserName $ config ^. nick



-- Env Typeclasses

class HasDefaultUserName a where
    getDefaultUser :: a -> UserName

instance HasDefaultUserName Env where
    getDefaultUser = defaultUserName . envConfig


class HasServerConfigs a where
    getServerConfigs :: a -> M.Map ServerName ServerConfig

instance HasServerConfigs Env where
    getServerConfigs = serverConfig . envConfig


-- | Get a reference to the ServerData map.
class HasServerMap a where
    getServerMap :: a -> TVar (M.Map ServerName ServerData)

instance HasServerMap Env where
    getServerMap = envServers


class HasClientQueues a where
    getClientQueues :: a -> TVar (M.Map ClientId ClientQueue)

instance HasClientQueues Env where
    getClientQueues = envClientQueues


class HasDaemonQueue a where
    getDaemonQueue :: a -> DaemonQueue

instance HasDaemonQueue Env where
    getDaemonQueue = envDaemonQueue


class HasIrcQueue a where
    getIrcQueue :: a -> IrcQueue

instance HasIrcQueue Env where
    getIrcQueue = envIrcQueue


class HasNextClientId a where
    getNextClientId :: a -> TVar ClientId

instance HasNextClientId Env where
    getNextClientId = envNextClientId

instance HasNextClientId (TVar ClientId) where
    getNextClientId = id


class HasSubscriptions a where
    getSubscriptions :: a -> TVar (M.Map ChannelId [ClientId])

instance HasSubscriptions Env where
    getSubscriptions = envSubscriptions



-- TYPES

type ServerM a
    = ReaderT Env IO a

data Env
    = Env
        { envConfig :: Config
        -- ^ The Static Application Configuration.
        , envServers :: TVar (M.Map ServerName ServerData)
        -- ^ Modifiable Data for Each Server
        , envClientQueues :: TVar (M.Map ClientId ClientQueue)
        -- ^ The Queues Containing Messages for each Client from the Daemon
        , envDaemonQueue :: DaemonQueue
        -- ^ A Queue Containing Messages for the Daemon from the Client
        , envIrcQueue :: IrcQueue
        -- ^ A Queue Containing Messages for the Daemon from an IRC Server
        , envNextClientId :: TVar ClientId
        -- ^ A Counter Tracking the Id for the Next Connecting Client
        , envSubscriptions :: TVar (M.Map ChannelId [ClientId])
        }


type IrcQueue
    = TQueue IrcMsg

data IrcMsg
    = ReceivedMessage ChannelId ChannelMessage
    deriving (Show)

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
        , serverCommands :: [Command]
        }

data ServerData
    = ServerData
        { channelMap :: M.Map ChannelName ChannelData
        , serverLog :: [ChannelMessage]
        , serverThread :: Maybe (Async (), IRCState ())
        }

data ServerSecurity
    = Plain
    | TLS

-- Newtype Wrappers

newtype Password
    = Password
        { getPassword :: T.Text
        }

newtype ServerHost
    = ServerHost
        { getServerHost :: B.ByteString
        }

newtype ServerPort
    = ServerPort
        { getServerPort :: Int
        }

newtype Command
    = Command
        { getCommand :: T.Text
        }


-- Config Parsing

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v ->
        Config
            <$> v .: "default-user-name"
            <*> v .: "servers"

instance FromJSON ServerConfig where
    parseJSON = withObject "ServerConfig" $ \v ->
        ServerConfig
            <$> v .:? "user-name"
            <*> v .:? "password"
            <*> v .: "host"
            <*> v .: "port"
            <*> parseSecurity v
            <*> (fromMaybe [] <$> v .:? "channels")
            <*> (fromMaybe [] <$> v .:? "commands")
        where
            parseSecurity v = do
                useTLS <- fromMaybe False <$> v .:? "use-tls"
                if useTLS then
                    return TLS
                else
                    return Plain

instance FromJSON Password where
    parseJSON = withText "Password" $ return . Password

instance FromJSON ServerHost where
    parseJSON = withText "ServerHost" $ return . ServerHost . encodeUtf8

instance FromJSON ServerPort where
    parseJSON = withScientific "ServerPort" $ return . ServerPort . floor

instance FromJSON Command where
    parseJSON = withText "Command" $ return . Command
