{-# LANGUAGE OverloadedStrings #-}

module ServerHandlers
  ( searchGearItems
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import qualified Data.Text as T
import System.Random ( getStdRandom, Random(randomR) )

import qualified DB
import qualified Types

mkSearch :: T.Text -> [a] -> Types.Search a
mkSearch = Types.Search

searchGearItems :: Monad m => Maybe T.Text -> m (Types.Search DB.GearItem)
searchGearItems Nothing = return (Types.Search "" [])
searchGearItems (Just q) = return (Types.Search q [])

--searchGearItems :: Monad m => T.Text -> m (Types.Search DB.GearItem)
--searchGearItems q = do
--  allItems <- DB.getAllGearItems
--  return (Types.Search q [])
