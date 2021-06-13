module SubmitterNode where

import qualified Submitter as S
import           SubmitterApi

import           Entity.Node (Node (..))
import           Entity.Port (Port (..))

import           Network.HTTP.Client      (Manager)
import           Network.Wai.Handler.Warp (runSettings, setBeforeMainLoop, setPort, defaultSettings)
import           RIO
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
                     :<|> S.getState submitter
                     :<|> S.forgetLeader submitter

    a <- async . runSettings settings $
        serve (Proxy :: Proxy SubmitterApi) submitterRoutes

    takeMVar ready

    pure a
