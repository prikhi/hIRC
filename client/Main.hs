{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Forms ((@@=), Form, newForm, editTextField, renderForm, handleFormEvent, formState, allFieldsValid)
import Brick.Widgets.Border (vBorder)
import Control.Concurrent.Async (Concurrently(..))
import Control.Concurrent.STM (atomically, newTQueueIO, writeTQueue)
import Control.Lens ((^.), (.~), (?~), (%~), makeLenses)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Conduit ((.|), ConduitT, runConduit, await)
import Data.Conduit.Serialization.Binary (conduitEncode, conduitDecode)
import Data.Conduit.Network.Unix (AppDataUnix, clientSettings, runUnixClient, appSource, appSink)
import Data.Conduit.TQueue (sourceTQueue)
import Data.Function ((&))
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
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
* Startup should display "Channel Selection" list, use to pick channel to
  subscribe to
* When switching between channels, should we retain the input form text, or
  keep the input form text on a per-channel basis?
* Server/Status log like in irssi
* Show Nicklist
* Show Session Bar - current time, username in current channel, subscribed
  channels(differentiate between hidden & shown)
* Colors - randomize color of other's usernames, bold mentions of our name,
           Brick theme so users can customize colors

-}


-- Types

data AppWidget
    = InputField
    deriving (Show, Eq, Ord)

type AppEvent
    = ClientMsg


newtype InputForm
    = InputForm
        { _input :: T.Text
        } deriving (Show)

makeLenses ''InputForm

data AppState
    = AppState
        { _appDaemonQueue :: DaemonQueue
        , _appChannelData :: M.Map ChannelId ChannelData
        , _appClientId :: Maybe ClientId
        , _appCurrentChannel :: Maybe ChannelId
        , _appInputForm :: Form InputForm AppEvent AppWidget
        }

makeLenses ''AppState

-- | Create a blank input form, labeled by the current channel's name.
inputForm :: Maybe ChannelId -> Form InputForm AppEvent AppWidget
inputForm maybeChannel =
    let
        channelName =
            maybe "$ " (\(ChannelId _ n) -> "[" <> getChannelName n <> "] ")
                maybeChannel
    in
        newForm
            [ (txt channelName <+>) @@= editTextField input InputField Nothing
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
                { _appDaemonQueue = daemonQueue
                , _appChannelData = M.empty
                , _appClientId = Nothing
                , _appCurrentChannel = Nothing
                , _appInputForm = inputForm Nothing
                }


-- | Connect to the Daemon's socket.
connectToDaemon :: DaemonQueue -> BChan ClientMsg -> IO ()
connectToDaemon daemonQueue clientChan = do
    socketPath <- getSocketPath
    runUnixClient (clientSettings socketPath) handler
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
app :: App AppState AppEvent AppWidget
app =
    App
        { appDraw = draw
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEvent
        , appStartEvent = return
        , appAttrMap = const colorMap
        }

colorMap :: AttrMap
colorMap =
    attrMap V.defAttr
        [ (topBarAttr, V.black `on` V.white)
        ]

topBarAttr :: AttrName
topBarAttr = "topBar"


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
                if allFieldsValid (s ^. appInputForm) then
                    case s ^. appCurrentChannel of
                        Nothing ->
                            continue s
                        Just channelId -> do
                            let message = _input . formState $ s ^. appInputForm
                            sendDaemonMessage s $
                                SendMessage SendMessageData
                                    { messageTarget = channelId
                                    , messageContents = message
                                    }
                            continue $
                                s & appInputForm .~ inputForm (Just channelId)
                else
                    continue s

            -- Update the Input form.
            _ -> do
                updatedForm <- handleFormEvent e $ s ^. appInputForm
                continue $ s & appInputForm .~ updatedForm
    AppEvent msg ->
        -- TODO: refactor into own function
        case msg of
            -- Just subscribe to all channels for now.
            -- TODO: Some way to select channels - either start blank w/
            -- ability to add channels to a view, or group channels & start
            -- based on flag.
            Hello HelloData { yourClientId, availableChannels } -> do
                let updatedState = s & appClientId ?~ yourClientId
                sendDaemonMessage updatedState $ Subscribe SubscribeData
                    { requestedChannels = availableChannels
                    }
                continue updatedState
            -- Add the new channels' data & switch if no channel selected
            Subscriptions SubscriptionsData { subscribedChannels } ->
                let
                    newChannel =
                        fst <$> listToMaybe (M.toList subscribedChannels)
                    (currentChannel, channelInput) =
                        case s ^. appCurrentChannel of
                            Nothing ->
                                (newChannel, inputForm newChannel)
                            Just _ ->
                                (s ^. appCurrentChannel, s ^. appInputForm)
                    reversedLogs =
                        fmap (\c -> c { messageLog = reverse $ messageLog c })
                            subscribedChannels
                in
                    continue $ s
                        & appChannelData %~ M.union reversedLogs
                        & appCurrentChannel .~ currentChannel
                        & appInputForm .~ channelInput
            -- Add Message to Channel's Log
            NewMessage NewMessageData { newMessageTarget, newMessage } ->
                continue $ s
                    & appChannelData %~
                        M.adjust
                            (\d ->
                                d { messageLog = messageLog d ++ [newMessage] }
                            )
                            newMessageTarget
            -- Update Channel Topic & Add to Log
            NewTopic NewMessageData { newMessageTarget, newMessage } ->
                continue $ s
                    & appChannelData %~ M.adjust
                        (\d -> d
                            { messageLog = messageLog d ++ [newMessage]
                            , channelTopic = ChannelTopic (messageText newMessage)
                            }
                        ) newMessageTarget
            -- Update Channel Topic
            InitialTopic channelId channelTopic ->
                continue $ s
                    & appChannelData %~ M.adjust
                        (\d -> d { channelTopic = channelTopic }) channelId

    _ ->
        -- Ignore the Mouse Up/Down Events
        continue s

-- | Send a message to the daemon if a ClientId is available.
sendDaemonMessage :: MonadIO m => AppState -> DaemonMsg -> m ()
sendDaemonMessage s m =
    maybeM (s ^. appClientId) $ \clientId ->
        liftIO $ atomically
            $ writeTQueue (s ^. appDaemonQueue)
            $ DaemonRequest clientId m

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
        [ renderTopBar s
        , padBottom Max $ renderMessageLog s
        , vLimit 1 $ renderForm $ s ^. appInputForm
        ]


renderTopBar :: AppState -> Widget AppWidget
renderTopBar s =
    let
        topicText =
            case currentChannelData s of
                Just ChannelData { channelTopic } ->
                    getChannelTopic channelTopic
                Nothing ->
                    ""
        channelName =
            case s ^. appCurrentChannel of
                Just (ChannelId _ (ChannelName name)) ->
                    "[" <> name <> "]"
                Nothing ->
                    ""
    in
        withAttr topBarAttr
            $ vLimit 1
            $ hBox
                [ txt channelName
                , padLeft (Pad 1) $ padRight Max $ txt topicText
                ]


-- | Render the Message Log for a Channel.
-- TODO: Use list widget for scrolling capability
renderMessageLog :: AppState -> Widget AppWidget
renderMessageLog s =
    case currentChannelData s of
        Just ChannelData { messageLog } ->
            vBox . nonEmptyLog $ map renderMessage messageLog
        Nothing ->
            hBox
                [ txt "Connecting to Daemon..."
                ]
    where
        -- TODO: Pull into client config
        maxNameLength = 12
        -- Render a single Channel Message
        renderMessage :: ChannelMessage -> Widget AppWidget
        renderMessage m =
            let
                line = case m of
                    ChatMessage {} ->
                        [ vLimit 1
                            $ hLimit maxNameLength
                            $ padLeft Max
                            $ txt
                            $ getUserName
                            $ messageUser m
                        , padLeftRight 1 $ txt ">"
                        , wrap $ messageText m
                        ]
                    TopicMessage {} ->
                        [ wrap $
                            getUserName (messageUser m) <>
                            " has set the channel topic to `" <>
                            messageText m <>
                            "`."
                        ]
            in
                padRight Max $ hBox $
                    [ str
                        $ formatTime defaultTimeLocale "%d.%H:%M:%S"
                        $ messageTime m
                    , padLeftRight 1 $ vLimit 1 vBorder
                    ] ++ line
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


currentChannelData :: AppState -> Maybe ChannelData
currentChannelData s =
    s ^. appCurrentChannel >>= flip M.lookup (s ^. appChannelData)
