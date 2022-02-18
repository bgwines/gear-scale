{-# LANGUAGE TypeOperators #-}

module Server
  ( runServer
  ) where

import Servant ( type (:<|>)(..) )

import qualified Network.Wai ( Application )
import qualified Network.Wai.Handler.Warp ( run, Port )
import qualified Servant
    ( serve,
      serveDirectoryWebApp,
      Server )
import qualified ServerAPI

server :: Servant.Server ServerAPI.FullAPI
server = (ServerAPI.randomPoint
    :<|> ServerAPI.searchBook)
    :<|> Servant.serveDirectoryWebApp "static"

app :: Network.Wai.Application
app = Servant.serve ServerAPI.fullApi server

runServer :: Network.Wai.Handler.Warp.Port -> IO ()
runServer port = Network.Wai.Handler.Warp.run port app
