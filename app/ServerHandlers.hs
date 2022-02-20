{-# LANGUAGE OverloadedStrings #-}

module ServerHandlers
  ( putGearItem
  , searchGearItems
  ) where

import Control.Monad.IO.Class ( MonadIO(..) )
import qualified Data.Text as T

import qualified DB
import qualified Ids
import qualified Types


searchGearItems :: MonadIO m => Maybe T.Text -> m [DB.GearItem]
searchGearItems Nothing = return []
searchGearItems (Just q) = liftIO DB.getAllGearItems

putGearItem :: MonadIO m => DB.GearItem -> m T.Text
putGearItem clientGearItem = do
  itemId <- Ids.generate
  let gearItem = DB.GearItem {
      DB._gearitemId            = itemId
    , DB._gearitemName          = DB._gearitemName          clientGearItem
    , DB._gearitemIsPersonal    = DB._gearitemIsPersonal    clientGearItem
    , DB._gearitemOz            = DB._gearitemOz            clientGearItem
    , DB._gearitemKind          = DB._gearitemKind          clientGearItem
    , DB._gearitemCreatorUserId = DB._gearitemCreatorUserId clientGearItem }
  liftIO $ DB.putGearItem gearItem
  return itemId
