{-# LANGUAGE OverloadedStrings #-}

module ServerHandlers
  ( putGearItem
  , searchGearItems
  ) where

import Control.Monad.IO.Class ( MonadIO(..) )
import qualified Data.Text as T

import qualified DB
import qualified Types


searchGearItems :: MonadIO m => Maybe T.Text -> m [DB.GearItem]
searchGearItems Nothing = return []
searchGearItems (Just q) = liftIO DB.getAllGearItems

putGearItem :: MonadIO m => DB.GearItem -> m () --T.Text
putGearItem gearItem = do
  liftIO $ DB.putGearItem gearItem
  return ()
