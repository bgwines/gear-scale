{-# LANGUAGE ExplicitNamespaces  #-}

module Server
  ( runServer
  ) where

import Servant ( type (:<|>)(..) )

import qualified Network.Wai
import qualified Network.Wai.Handler.Warp
import qualified Servant
import qualified ServerAPI
import qualified ServerHandlers


server :: Servant.Server ServerAPI.FullAPI
server =
    (    ServerHandlers.putGearItem
    :<|> ServerHandlers.searchGearItems )

app :: Network.Wai.Application
app = Servant.serve ServerAPI.fullApi server

runServer :: Network.Wai.Handler.Warp.Port -> IO ()
runServer port = Network.Wai.Handler.Warp.run port app
