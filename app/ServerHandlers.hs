{-# LANGUAGE OverloadedStrings #-}

module ServerHandlers
  ( randomPoint
  , searchPoint
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import qualified Data.Text as T
import System.Random ( getStdRandom, Random(randomR) )
import qualified Types

mkSearch :: T.Text -> [a] -> Types.Search a
mkSearch = Types.Search

points :: [Types.Point]
points =
  [ Types.Point 0.0 0.0
  , Types.Point 0.1 0.1
  , Types.Point 0.2 0.2
  , Types.Point 0.3 0.3
  , Types.Point 0.4 0.4
  , Types.Point 0.5 0.5
  ]

searchPoint :: Monad m => Maybe T.Text -> m (Types.Search Types.Point)
searchPoint Nothing  = return (Types.Search "" points)
searchPoint (Just q) = return (Types.Search q points')
  where points' = filter (\p -> q `T.isInfixOf` (T.pack $ show p)) points

randomPoint :: MonadIO m => m Types.Point
randomPoint = liftIO . getStdRandom $ \g ->
  let (rx, g')  = randomR (-1, 1) g
      (ry, g'') = randomR (-1, 1) g'
  in (Types.Point rx ry, g'')
