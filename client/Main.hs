{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Forms ((@@=), Form, newForm, editTextField, renderForm, handleFormEvent, formState, allFieldsValid)
import Brick.Widgets.Border (vBorder)
import Control.Arrow (second)
import Control.Concurrent.Async (Concurrently(..))
import Control.Concurrent.STM (atomically, newTQueueIO, writeTQueue)
import Control.Lens (makeLenses)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Conduit ((.|), ConduitT, runConduit, await)
import Data.Conduit.Serialization.Binary (conduitEncode, conduitDecode)
import Data.Conduit.Network.Unix (AppDataUnix, clientSettings, runUnixClient, appSource, appSink)
import Data.Conduit.TQueue (sourceTQueue)
import Data.Maybe (listToMaybe)
import Data.Time (formatTime, defaultTimeLocale)
import Text.Wrap (WrapSettings(breakLongWords), defaultWrapSettings)

import qualified Control.Immortal as Immortal
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Graphics.Vty as V

import HIrc

{-

TODO:

* There's some functionality repeated in `handleEvent` & the server(like
  channel log updates). Should move those typeclasses from server to common
  & use them here.
* Introduce the idea of "Panes"(horizontal/vertical split windows). Add
  keybindings to open/close/navigate panes as well as changing the channel
  shown in the current pane
* Add usernames to message logs
* Server/Status log like in irssi
* Show Bar with Channel Name & Topic
* Show Topic & Nicklist
* Show Session Bar - current time, username in current channel, subscribed
  channels(differentiate between hidden & shown)

-}


-- Types

data AppWidget
    = InputField
    deriving (Show, Eq, Ord)

type AppEvent
    = ClientMsg

-- | TODO: Add per-channel Input Forms
data AppState
    = AppState
        { appDaemonQueue :: DaemonQueue
        , appChannelData :: ChannelData
        , appClientId :: Maybe ClientId
        , appCurrentChannel :: Maybe (ServerName, ChannelName)
        , appInputForm :: Form InputForm AppEvent AppWidget
        }

-- TODO use better datatype for appending to end of message list.
type ChannelData
    = M.Map (ServerName, ChannelName) [ChatMessage]

newtype InputForm
    = InputForm
        { _input :: T.Text
        } deriving (Show)

makeLenses ''InputForm

-- TODO: Prepend w/ Channel Name: `[#xmonad] Hello world!`
inputForm :: Form InputForm AppEvent AppWidget
inputForm =
    newForm
     [ (txt "$ " <+>) @@= editTextField input InputField Nothing
     ]
     (InputForm "")


-- | Create the Communication Channels, Connect to the Daemon & Run the UI.
main :: IO ()
main = do
    clientChan <- newBChan 100
    daemonQueue <- newTQueueIO
    void . Immortal.create $ const
        $ connectToDaemon daemonQueue clientChan
    void $ customMain defVty (Just clientChan) app (state daemonQueue)
    where
        defVty =
            V.mkVty V.defaultConfig
        state daemonQueue =
            AppState
                { appDaemonQueue = daemonQueue
                , appChannelData = M.empty
                , appClientId = Nothing
                , appCurrentChannel = Nothing
                , appInputForm = inputForm
                }


-- | Connect to the Daemon's socket.
-- TODO: Socket should live in XDG cache or data dir.
connectToDaemon :: DaemonQueue -> BChan ClientMsg -> IO ()
connectToDaemon daemonQueue clientChan =
    runUnixClient (clientSettings "hircd.sock") handler
    where
        -- Connect the Client TQueue & BChan to the Daemon Socket.
        handler :: AppDataUnix -> IO ()
        handler connection =
            runConcurrently
                $ Concurrently (runConduit $ sourceTQueue daemonQueue .| conduitEncode .| appSink connection)
                *> Concurrently (runConduit $ appSource connection .| conduitDecode .| sinkBChan)
        -- Sink a conduit into the Brick BChan.
        sinkBChan :: ConduitT ClientMsg o IO ()
        sinkBChan =
            await >>= \case
                Nothing ->
                    return ()
                Just clientMsg ->
                    liftIO (writeBChan clientChan clientMsg) >> sinkBChan


-- | Configure the Brick UI.
-- TODO: Colors!
app :: App AppState AppEvent AppWidget
app =
    App
        { appDraw = draw
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEvent
        , appStartEvent = return
        , appAttrMap = const $ attrMap V.defAttr []
        }


-- Update

-- | Handle Keypress Events & Messages from the Daemon.
handleEvent :: AppState -> BrickEvent AppWidget AppEvent -> EventM AppWidget (Next AppState)
handleEvent s = \case
    e@(VtyEvent ev) ->
        case ev of
            -- Say Goodbye to the Daemon & Stop the Application.
            V.EvKey (V.KChar 'q') [V.MCtrl] ->
                sendDaemonMessage s Goodbye >> halt s
            -- Send Chat Message to Channel if one is available.
            V.EvKey V.KEnter [] ->
                if allFieldsValid (appInputForm s) then
                    case appCurrentChannel s of
                        Nothing ->
                            continue s
                        Just (serverName, channelName) -> do
                            let message = _input . formState $ appInputForm s
                            sendDaemonMessage s $
                                SendMessage SendMessageData
                                    { messageTarget = (serverName, channelName)
                                    , messageContents = message
                                    }
                            continue s { appInputForm = inputForm }
                else
                    continue s

            -- Update the Input form.
            _ -> do
                updatedForm <- handleFormEvent e $ appInputForm s
                continue s { appInputForm = updatedForm }
    AppEvent msg ->
        case msg of
            -- Just subscribe to all channels for now.
            -- TODO: Some way to select channels - either start blank w/
            -- ability to add channels to a view, or group channels & start
            -- based on flag.
            Hello HelloData { yourClientId, availableChannels } -> do
                let updatedState = s { appClientId = Just yourClientId }
                sendDaemonMessage updatedState $ Subscribe SubscribeData
                    { requestedChannels = availableChannels
                    }
                continue updatedState
            -- Update the Message Logs.
            -- TODO: Insert only new channel's logs so we can send Subscribe
            -- events for new channels without replacing existing logs.
            Subscriptions SubscriptionsData { subscriptionLogs } ->
                continue s
                    { appChannelData = M.fromList $ map (second reverse) subscriptionLogs
                    , appCurrentChannel = fst <$> listToMaybe subscriptionLogs
                    }
            -- Add Message to Channel's Log
            NewMessage NewMessageData { newMessageTarget, newMessage } ->
                continue s
                    { appChannelData =
                        M.adjust (++ [newMessage]) newMessageTarget $ appChannelData s
                    }
    _ ->
        -- Ignore the Mouse Up/Down Events
        continue s

-- | Send a message to the daemon if a ClientId is available.
sendDaemonMessage :: MonadIO m => AppState -> DaemonMsg -> m ()
sendDaemonMessage s m =
    maybeM (appClientId s) $ \clientId ->
        liftIO $ atomically $ writeTQueue (appDaemonQueue s) $ DaemonRequest clientId m

maybeM :: Monad m => Maybe a -> (a -> m ()) -> m ()
maybeM m f =
    maybe (return ()) f m


-- Render

-- | Render the UI.
draw :: AppState -> [Widget AppWidget]
draw s =
    [ renderCurrentView s
    ]

-- | Render the Currently Selected Channel.
renderCurrentView :: AppState -> Widget AppWidget
renderCurrentView s =
    vBox
        [ padBottom Max $ renderMessageLog s
        , vLimit 1 $ renderForm (appInputForm s)
        ]


-- | Render the Message Log for a Channel.
-- TODO: Use list widget for scrolling capability
renderMessageLog :: AppState -> Widget AppWidget
renderMessageLog s =
    case appCurrentChannel s >>= flip M.lookup (appChannelData s) of
        Just messages ->
            vBox . nonEmptyLog $ map renderMessage messages
        Nothing ->
            hBox
                [ txt "Connecting to Daemon..."
                ]
    where
        -- Render a single Channel Message
        renderMessage :: ChatMessage -> Widget AppWidget
        renderMessage m =
            padRight Max $ hBox
                [ str
                    $ formatTime defaultTimeLocale "%d.%H:%M:%S"
                    $ messageTime m
                , padLeftRight 1 $ vLimit 1 vBorder
                , wrap $ messageText m
                ]
        -- Wrap text, breaking inside words if necessary
        wrap :: T.Text -> Widget AppWidget
        wrap =
            txtWrapWith (defaultWrapSettings { breakLongWords = True })
        -- Show an empty widget if the log is empty.
        nonEmptyLog :: [Widget AppWidget] -> [Widget AppWidget]
        nonEmptyLog l =
            if null l then
                [ txt " " ]
            else
                l
