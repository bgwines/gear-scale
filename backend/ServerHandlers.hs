{-# LANGUAGE OverloadedStrings #-}

module ServerHandlers
  ( putGearItem
  , searchGearItems
  ) where

import Control.Monad.IO.Class ( MonadIO(..) )
import qualified Data.Text as T

import qualified ClientTypes
import qualified DB
import qualified Ids

serverGearItemToClient :: DB.GearItem -> ClientTypes.GearItem
serverGearItemToClient item = ClientTypes.GearItem
    { ClientTypes.itemId        = DB._gearitemId            item
    , ClientTypes.name          = DB._gearitemName          item
    , ClientTypes.isPersonal    = DB._gearitemIsPersonal    item
    , ClientTypes.oz            = DB._gearitemOz            item
    , ClientTypes.kind          = DB._gearitemKind          item
    , ClientTypes.creatorUserId = DB._gearitemCreatorUserId item
    }

searchGearItems :: MonadIO m => Maybe T.Text -> m [ClientTypes.GearItem]
searchGearItems Nothing = return []
searchGearItems (Just q) = do
  liftIO $ putStrLn $ "/searchGearItems?q=" ++ T.unpack q
  map serverGearItemToClient <$> liftIO DB.getAllGearItems

putGearItem :: MonadIO m => ClientTypes.GearItem -> m T.Text
putGearItem clientGearItem = do
  liftIO $ putStrLn $ "/putGearItem body=" ++ show clientGearItem

  let clientItemId = ClientTypes.itemId clientGearItem
  let isNew = clientItemId == ""
  newItemId <- Ids.generate
  let itemId = if isNew then newItemId else clientItemId
  let gearItem = DB.GearItem {
      DB._gearitemId            = itemId
    , DB._gearitemName          = ClientTypes.name          clientGearItem
    , DB._gearitemIsPersonal    = ClientTypes.isPersonal    clientGearItem
    , DB._gearitemOz            = ClientTypes.oz            clientGearItem
    , DB._gearitemKind          = ClientTypes.kind          clientGearItem
    , DB._gearitemCreatorUserId = ClientTypes.creatorUserId clientGearItem
    }

  let insertItem = if isNew then DB.putGearItem else DB.updateGearItem
  liftIO $ insertItem gearItem
  return itemId
