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
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Control.Concurrent               (threadDelay)
import           Control.Monad                    (forever)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client              (Manager, defaultManagerSettings, newManager)

import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes

bothWindows :: Widget ()
bothWindows = withBorderStyle unicode
            . borderWithLabel (str "Hello!")
            $ leftWindow <+> vBorder <+> rightWindow

leftWindow =
    center $ str "Left"

rightWindow =
    center $ str "Right"

-- https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst

main :: IO ()
main = do

    let app = App { appDraw         = draw
                  , appChooseCursor = chooseCursor
                  , appHandleEvent  = handleEvent
                  , appStartEvent   = startEvent
                  , appAttrMap      = attrs
                  }
                  
        initialState = AppState 0
  
    finalState <- defaultMain app initialState

    pure ()

data Tick = Tick deriving Show

data Name = Name
    deriving (Eq, Ord, Show)

step (AppState n) = (AppState $ n +1 )

data AppState = AppState !Int

draw :: AppState -> [Widget Name]
draw appState = [foo appState]

chooseCursor :: s -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor _ _ = Nothing

handleEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvent g (AppEvent Tick) = continue $ step g
handleEvent g (VtyEvent (EvResize _ _)) = continue $ step g
handleEvent g (VtyEvent (EvKey _ [])) = continue $ step g

handleEvent g (VtyEvent (EvKey (KChar 'c') [MCtrl])) = error "goodbye"

handleEvent g x = error $ show x

startEvent :: s -> EventM Name s
startEvent = pure

attrs :: s -> AttrMap
attrs _ = attrMap boring [ ]

boring =
        Attr { attrStyle     = Default
             , attrForeColor = Default
             , attrBackColor = Default
             , attrURL       = Default
             }

foo (AppState n)=
    withBorderStyle unicode $
      borderWithLabel (str $ "Hello " ++ show n) $
        (center (str "Left") <+> vBorder <+> center (str "Right"))

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