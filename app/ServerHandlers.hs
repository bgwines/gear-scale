{-# LANGUAGE OverloadedStrings #-}

module ServerHandlers
  ( randomPoint
  , searchBook
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import qualified Data.Text as T
import System.Random ( getStdRandom, Random(randomR) )
import qualified Types

mkSearch :: T.Text -> [a] -> Types.Search a
mkSearch = Types.Search

book :: T.Text -> T.Text -> Int -> Types.Book
book = Types.Book

books :: [Types.Book]
books =
  [ book "Paul Hudak" "The Haskell School of Expression: Learning Functional Programming through Multimedia" 2000
  , book "Bryan O'Sullivan, Don Stewart, and John Goerzen" "Real World Haskell" 2008
  , book "Miran Lipovača" "Learn You a Haskell for Great Good!" 2011
  , book "Graham Hutton" "Programming in Haskell" 2007
  , book "Simon Marlow" "Parallel and Concurrent Programming in Haskell" 2013
  , book "Richard Bird" "Introduction to Functional Programming using Haskell" 1998
  ]

searchBook :: Monad m => Maybe T.Text -> m (Types.Search Types.Book)
searchBook Nothing  = return (Types.Search "" books)
searchBook (Just q) = return (Types.Search q books')

  where books' = filter (\b -> q' `T.isInfixOf` T.toLower (Types.author b)
                            || q' `T.isInfixOf` T.toLower (Types.title b)
                        )
                        books
        q' = T.toLower q

randomPoint :: MonadIO m => m Types.Point
randomPoint = liftIO . getStdRandom $ \g ->
  let (rx, g')  = randomR (-1, 1) g
      (ry, g'') = randomR (-1, 1) g'
  in (Types.Point rx ry, g'')
