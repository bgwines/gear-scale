{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( runServer
  , writeJSFiles
  ) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Aeson ( ToJSON )
import Data.Proxy ( Proxy(..) )
import Data.Text as T (Text)
import Data.Text.IO as T (writeFile, readFile)
import GHC.Generics ( Generic )
import Language.Javascript.JQuery ( file )
import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run, Port )
import qualified Data.Text as T
import Servant
    ( serve,
      serveDirectoryWebApp,
      type (:<|>)(..),
      JSON,
      QueryParam,
      Raw,
      type (:>),
      Get,
      Server )
import GHC.IO ()
import Servant.JS ( jsForAPI, jquery )
import System.Random ( getStdRandom, Random(randomR) )

type LimitedAPI
  = "point" :> Get '[JSON] Point
    :<|> "books" :> QueryParam "q" Text :> Get '[JSON] (Search Book)

type FullAPI = LimitedAPI :<|> Raw

-------------------------------------------------------------------

data Point = Point
  { x :: Double
  , y :: Double
  } deriving Generic

instance ToJSON Point


data Search a = Search
  { query   :: Text
  , results :: [a]
  } deriving Generic

mkSearch :: Text -> [a] -> Search a
mkSearch = Search

instance ToJSON a => ToJSON (Search a)

data Book = Book
  { author :: Text
  , title  :: Text
  , year   :: Int
  } deriving Generic

instance ToJSON Book

book :: Text -> Text -> Int -> Book
book = Book

books :: [Book]
books =
  [ book "Paul Hudak" "The Haskell School of Expression: Learning Functional Programming through Multimedia" 2000
  , book "Bryan O'Sullivan, Don Stewart, and John Goerzen" "Real World Haskell" 2008
  , book "Miran Lipovača" "Learn You a Haskell for Great Good!" 2011
  , book "Graham Hutton" "Programming in Haskell" 2007
  , book "Simon Marlow" "Parallel and Concurrent Programming in Haskell" 2013
  , book "Richard Bird" "Introduction to Functional Programming using Haskell" 1998
  ]

searchBook :: Monad m => Maybe Text -> m (Search Book)
searchBook Nothing  = return (mkSearch "" books)
searchBook (Just q) = return (mkSearch q books')

  where books' = filter (\b -> q' `T.isInfixOf` T.toLower (author b)
                            || q' `T.isInfixOf` T.toLower (title b)
                        )
                        books
        q' = T.toLower q

randomPoint :: MonadIO m => m Point
randomPoint = liftIO . getStdRandom $ \g ->
  let (rx, g')  = randomR (-1, 1) g
      (ry, g'') = randomR (-1, 1) g'
  in (Point rx ry, g'')

home :: MonadIO m => m Point
home = liftIO . getStdRandom $ \g ->
  let (rx, g')  = randomR (-1, 1) g
      (ry, g'') = randomR (-1, 1) g'
  in (Point rx ry, g'')

limitedApi :: Proxy LimitedAPI
limitedApi = Proxy

fullApi :: Proxy FullAPI
fullApi = Proxy

server :: Server FullAPI
server = (randomPoint
    :<|> searchBook)
    :<|> serveDirectoryWebApp "static"

app :: Application
app = serve fullApi server

runServer :: Port -> IO ()
runServer port = run port app

--------
-- JS --
--------

apiJS :: Text
apiJS = jsForAPI limitedApi jquery

writeJSFiles :: IO ()
writeJSFiles = do
  T.writeFile "static/api.js" apiJS
  jq <- T.readFile =<< Language.Javascript.JQuery.file
  T.writeFile "static/jq.js" jq
