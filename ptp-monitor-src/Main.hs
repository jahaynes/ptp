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


bothWindows :: Widget ()
bothWindows = withBorderStyle unicode
            . borderWithLabel (str "Hello!")
            $ leftWindow <+> vBorder <+> rightWindow

leftWindow =
    center $ str "Left"

rightWindow =
    center $ str "Right"

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

    let app = App { }
        initialState = undefined
  
    -- finalState <- defaultMain app initialState
  
    pure ()
    -- Use finalState and exit


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