{-# LANGUAGE LambdaCase #-}

module Main where

import Client.SubmitterClient
import Entity.Id
import Entity.Host
import Entity.Node
import Entity.Port
import Entity.SequenceNum
import Entity.Topic
import Requests.State

import           Brick
import           Brick.BChan (BChan, newBChan, writeBChan)
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style

import           Control.Concurrent               (forkIO, threadDelay)
import           Control.Monad                    (forever, void)
import           Control.Monad.IO.Class           (liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client              (Manager, defaultManagerSettings, newManager)

import Graphics.Vty
import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes

instance ToJSON Host
instance ToJSON Id
instance ToJSON Node
instance ToJSON Port
instance ToJSON SequenceNum
instance ToJSON Topic
instance ToJSON TopicState

-- https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst

main :: IO ()
main = do

    chan <- newBChan 10

    let initialState = AppState { count = 0 
                                , status = Running
                                }
  
    let mkHandle = mkVty defaultConfig
    handle <- mkHandle
    _ <- forkIO $ timerThread chan
    void (customMain handle mkHandle (Just chan) app initialState)

    where
    timerThread :: BChan Tick -> IO b
    timerThread chan = forever $ do
        threadDelay 1000000
        writeBChan chan Tick

data Tick = Tick deriving Show

data Name = Name
    deriving (Eq, Ord, Show)

app :: App AppState Tick Name
app = App { appDraw         = draw
          , appChooseCursor = chooseCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = startEvent
          , appAttrMap      = attrs
          }

    where
    draw :: AppState -> [Widget Name]
    draw appState = [foo]

        where
        foo =
            withBorderStyle unicode $
                borderWithLabel (str $ "Hello " ++ show (count appState)) $
                    (center (str "Left") <+> vBorder <+> center (str "Right"))

    chooseCursor :: s -> [CursorLocation Name] -> Maybe (CursorLocation Name)
    chooseCursor _ _ = Nothing

    handleEvent :: AppState
                -> BrickEvent Name Tick
                -> EventM Name (Next AppState)
    handleEvent appState (AppEvent Tick)                        = continue $ step appState
    handleEvent appState (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt appState
    handleEvent appState (VtyEvent (EvResize _ _))              = continue appState
    handleEvent appState (VtyEvent (EvKey _ []))                = continue appState
    handleEvent        _ x = error $ show x

    startEvent :: s -> EventM Name s
    startEvent = pure

    attrs :: s -> AttrMap
    attrs _ = attrMap boring [ ]
        where
        boring =
            Attr { attrStyle     = Default
                 , attrForeColor = Default
                 , attrBackColor = Default
                 , attrURL       = Default
                 }

step :: AppState -> AppState
step appState = appState { count = count appState + 1 }

data AppState =
    AppState { count  :: !Int
             , status :: !Status
             }

data Status = Running
            | Exiting

{-
    http <- newManager defaultManagerSettings



    let stateClient = stateBuilder http (Node (Id "whatever") localHost (Port 8180))

    forever $ do

        threadDelay 500000

        stateClient >>= \case

            Left e -> error $ show e

            Right (StateResponse xs) ->
                L8.putStrLn $ encode xs
-}
