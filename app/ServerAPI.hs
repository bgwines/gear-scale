{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module ServerAPI
  ( LimitedAPI
  , FullAPI
  , limitedApi
  , fullApi
  , randomPoint
  , searchBook
  ) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Aeson ( ToJSON )
import Data.Proxy ( Proxy(..) )
import GHC.Generics ( Generic )
import qualified Data.Text as T
import System.Random ( getStdRandom, Random(randomR) )
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


type LimitedAPI
  = "point" :> Get '[JSON] Point
    :<|> "books" :> QueryParam "q" T.Text :> Get '[JSON] (Search Book)

type FullAPI = LimitedAPI :<|> Raw

limitedApi :: Proxy LimitedAPI
limitedApi = Proxy

fullApi :: Proxy FullAPI
fullApi = Proxy

data Point = Point
  { x :: Double
  , y :: Double
  } deriving Generic

instance ToJSON Point


data Search a = Search
  { query   :: T.Text
  , results :: [a]
  } deriving Generic

mkSearch :: T.Text -> [a] -> Search a
mkSearch = Search

instance ToJSON a => ToJSON (Search a)

data Book = Book
  { author :: T.Text
  , title  :: T.Text
  , year   :: Int
  } deriving Generic

instance ToJSON Book

book :: T.Text -> T.Text -> Int -> Book
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

searchBook :: Monad m => Maybe T.Text -> m (Search Book)
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
