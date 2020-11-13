module SubmitterNode where

import qualified Submitter as S
import           SubmitterApi

import           Entity.Node (Node (..))
import           Entity.Port (Port (..))

import           Control.Concurrent       (newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent.Async (Async, async)
import           Network.HTTP.Client      (Manager)
import           Network.Wai.Handler.Warp (runSettings, setBeforeMainLoop, setPort, defaultSettings)
import           Servant

create :: Manager
       -> Node
       -> IO (Async ())
create http node = do

    submitter <- S.create node http

    ready <- newEmptyMVar

    let Port p = getPort node

    let settings = setPort p
                 . setBeforeMainLoop (putMVar ready ())
                 $ defaultSettings

        submitterRoutes = S.createTopic submitter
                     :<|> S.sync submitter
                     :<|> S.submit submitter

    a <- async . runSettings settings $
        serve (Proxy :: Proxy SubmitterApi) submitterRoutes

    takeMVar ready

    pure a
