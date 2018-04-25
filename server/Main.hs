{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, modifyTVar)
import Control.Lens ((&), (.~), (%~))
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT, runReaderT, liftIO)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Network.IRC.Client
    ( plainConnection, defaultInstanceConfig, runClient, logfunc, stdoutLogger
    , channels, nick, password, send, Message(..), onconnect
    )

import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text as T

main :: IO ()
main = do
    env <- initialize
    flip runReaderT env $ forever $ liftIO (threadDelay 1000000)


initialize :: IO Env
initialize = do
    let config = testConfig
    servers <- connectToServers config
    return $ Env config servers

connectToServers :: Config -> IO Servers
connectToServers (Config name autoMap) =
    fmap Servers . sequence $ M.map (connectToServer name) autoMap

connectToServer :: UserName -> ServerConfig -> IO (TVar ServerData)
connectToServer defaultName config = do
    dataTVar <- newTVarIO $ ServerData M.empty [] Nothing
    let name = fromMaybe defaultName $ userName config
        host = serverHost config
        port = serverPort config
        conn = plainConnection (getServerHost host) (getServerPort port)
            & logfunc .~ stdoutLogger
            & password .~ (getPassword <$> serverPassword config)
            & onconnect %~ (>> identify name)
        cfg = defaultInstanceConfig (getUserName name)
            & nick .~ getUserName name
            & channels .~ map getChannelName (defaultChannels config)
    clientThread <- async $ runClient conn cfg ()
    atomically . modifyTVar dataTVar $ \d -> d { serverThread = Just clientThread }
    return dataTVar
    where
        -- TODO: ServerConfig should specify these commands because they are dependent on the IRC server
        identify name =
            case serverPassword config of
                Just (Password pass) -> do
                    send . RawMsg
                        $ "ACC REGISTER " <> getUserName name <> " * passphrase :" <> pass
                    send . Privmsg "NickServ" . Right
                        $ "IDENTIFY " <> getUserName name <> " " <> pass
                Nothing ->
                    return ()



-- TYPES

-- TODO: connect to servers, connect to channels, store connections & data
-- in tvars, make message queues between main thread & server threads,
-- start socket server.


type ServerM a
    = ReaderT Env IO a

data Env
    = Env
        { envConfig :: Config
        , envServers :: Servers
        }

data Config
    = Config
        { defaultUserName :: UserName
        , serverConfig :: M.Map ServerName ServerConfig
        }

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
        , serverThread :: Maybe (Async ())
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

newtype Servers
    = Servers
        { serverMap :: M.Map ServerName (TVar ServerData)
        }

newtype UserName
    = UserName
        { getUserName :: T.Text
        }

newtype Password
    = Password
        { getPassword :: T.Text
        }

newtype ChatMessage
    = Message
        { getMessage :: T.Text
        }

newtype ServerName
    = ServerName
        { getServerName :: T.Text
        } deriving (Eq, Ord)

newtype ChannelName
    = ChannelName
        { getChannelName :: T.Text
        } deriving (Eq, Ord)

newtype ServerHost
    = ServerHost
        { getServerHost :: B.ByteString
        }

newtype ServerPort
    = ServerPort { getServerPort :: Int
        }
