{-# LANGUAGE OverloadedStrings #-}

module ServerHandlers
  ( putGearItem
  , deleteGearItem
  , searchGearItems
  , putTrip
  , searchTrips
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

serverTripToClient :: DB.Trip -> ClientTypes.Trip
serverTripToClient trip = ClientTypes.Trip
    { ClientTypes.tripId            = DB._tripId            trip
    , ClientTypes.tripName          = DB._tripName          trip
    , ClientTypes.tripCreatorUserId = DB._tripCreatorUserId trip
    , ClientTypes.tripMemberUserIdsCommaSeparated = "" -- TODO
    }

searchGearItems :: MonadIO m => Maybe T.Text -> m [ClientTypes.GearItem]
searchGearItems Nothing = return []
searchGearItems (Just q) = do
  liftIO $ putStrLn $ "/searchGearItems?q=" ++ T.unpack q
  map serverGearItemToClient <$> liftIO DB.getAllGearItems

-- TODO: error handling on all server endpoints
deleteGearItem :: MonadIO m => T.Text -> m Bool
deleteGearItem itemId = do
  liftIO $ putStrLn $ "/deleteGearItem itemId=" ++ T.unpack itemId
  liftIO $ DB.deleteGearItem itemId
  return True

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

putTrip :: MonadIO m => ClientTypes.Trip -> m T.Text
putTrip clientGearItem = undefined

searchTrips :: MonadIO m => Maybe T.Text -> m [ClientTypes.Trip]
searchTrips Nothing = return []
searchTrips (Just q) = do
  -- TODO: filter by q
  liftIO $ putStrLn $ "/searchTrips?q=" ++ T.unpack q
  map serverTripToClient <$> liftIO DB.getAllTrips
