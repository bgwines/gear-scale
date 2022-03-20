{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DB
  ( Db
  , db
  , putTrip
  , updateTrip
  , deleteTrip
  , putGearItem
  , updateGearItem
  , deleteGearItem
  , getAllGearItems
  , getGearItemById
  , clearAllGearItems
  , GearItemT(..)
  , GearItem(..)
  , GearItemId(..)
  , ClientTypes.GearKind(..)
  ) where

import Database.Beam
    ( Generic,
      Identity,
      delete,
      insert,
      update,
      insertValues,
      save,
      runDelete,
      runInsert,
      runUpdate,
      runSelectReturningOne,
      runSelectReturningList,
      val_,
      select,
      all_,
      filter_,
      just_,
      lookup_,
      defaultDbSettings,
      (==.),
      (/=.),
      Beamable,
      Columnar,
      Database,
      DatabaseSettings,
      Table(..),
      TableEntity )
import Database.Beam.Backend.SQL
    ( HasSqlValueSyntax(..)
    , autoSqlValueSyntax
    , FromBackendRow
    , fromBackendRow
    )
import Database.Beam.Sqlite ( runBeamSqlite, Sqlite )
import Database.SQLite.Simple ( open )
import Data.Text (Text, unpack)
import Data.Aeson ( ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions )

import qualified ClientTypes

--------------
-- GearKind --
--------------

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ClientTypes.GearKind where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite ClientTypes.GearKind where
  fromBackendRow = read . unpack <$> fromBackendRow

---------------
-- GearItemT --
---------------

data GearItemT f
    = GearItem
    { _gearitemId            :: Columnar f Text
    , _gearitemName          :: Columnar f Text
    , _gearitemIsPersonal    :: Columnar f Bool
    , _gearitemOz            :: Columnar f Double
    , _gearitemKind          :: Columnar f ClientTypes.GearKind
    , _gearitemCreatorUserId :: Columnar f Text }
    deriving Generic
instance Beamable GearItemT

type GearItem = GearItemT Identity
deriving instance Show GearItem
deriving instance Read GearItem
deriving instance Eq GearItem
instance ToJSON GearItem where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON GearItem

type GearItemId = PrimaryKey GearItemT Identity

instance Table GearItemT where
   data PrimaryKey GearItemT f = GearItemId (Columnar f Text) deriving (Generic, Beamable)
   primaryKey = GearItemId . _gearitemId

-----------
-- TripT --
-----------

data TripT f
    = Trip
    { _tripId            :: Columnar f Text
    , _tripName          :: Columnar f Text
    , _tripCreatorUserId :: Columnar f Text }
    deriving Generic
instance Beamable TripT

type Trip = TripT Identity
deriving instance Show Trip
deriving instance Read Trip
deriving instance Eq Trip
instance ToJSON Trip where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Trip

type TripId = PrimaryKey TripT Identity

instance Table TripT where
   data PrimaryKey TripT f = TripId (Columnar f Text) deriving (Generic, Beamable)
   primaryKey = TripId . _tripId

------------------
-- TripMembersT --
------------------

data TripMembersT f
    = TripMembers
    { _tripmembersTripId         :: Columnar f Text
    , _tripmembersMemberUserName :: Columnar f Text }
    deriving Generic
instance Beamable TripMembersT

type TripMembers = TripMembersT Identity
deriving instance Show TripMembers
deriving instance Read TripMembers
deriving instance Eq TripMembers
instance ToJSON TripMembers where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TripMembers

type TripMembersId = PrimaryKey TripMembersT Identity

instance Table TripMembersT where
   data PrimaryKey TripMembersT f = TripMembersId (Columnar f Text) (Columnar f Text) deriving (Generic, Beamable)
   primaryKey t = TripMembersId (_tripmembersTripId t) (_tripmembersMemberUserName t)



--------------------
-- GearSelectionT --
--------------------

--data GearSelection = GearSelection
--  { item :: GearItem
--  , selected :: Bool
--  , isPersonalOverride :: Maybe Bool
--  , inPack :: Bool
--  , isOptional :: Bool
--  , quantity :: Int
--  } deriving Generic

--data GearSelectionT f
--    = GearSelection
--    { _gearselectionId                 :: Columnar f Text
--    , _gearselectionGearItemId         :: Columnar f Text
--    , _gearselectionSelected           :: Columnar f Bool
--    , _gearselectionIsPersonalOverride :: Columnar f (Maybe Bool)
--    , _gearselectionInPack             :: Columnar f Bool
--    , _gearselectionIsOptional         :: Columnar f Bool
--    , _gearselectionQuantity           :: Columnar f Int }
--    deriving Generic
--instance Beamable GearSelectionT
--type GearSelection = GearSelectionT Identity
--deriving instance Show GearSelection
--deriving instance Eq GearSelection
--instance FromJSON GearSelection
--instance ToJSON GearSelection where
--  toEncoding = genericToEncoding defaultOptions

--------
-- DB --
--------

data Db f = Db
  { _gear_items :: f (TableEntity GearItemT)
  , _trips      :: f (TableEntity TripT)
  }
  deriving (Generic, Database be)

db :: DatabaseSettings be Db
db = defaultDbSettings

dbName :: String
dbName = "gear_scale.db"

------------
-- DB API --
------------

-- trips

-- TODO: implement
deleteTrip :: Text -> IO ()
deleteTrip tripId = do
  putStrLn "Delete not implemented"

updateTrip :: Trip -> IO ()
updateTrip trip = do
  putStrLn "Update not implemented"
  -- conn <- open dbName
  -- runBeamSqlite conn $ runUpdate $ save (_trips db) trip

putTrip :: Trip -> IO ()
putTrip trip = do
  putStrLn "Put not implemented"
  -- conn <- open dbName
  -- runBeamSqlite conn $ runInsert $ insert (_trips db) $ insertValues [ trip ]

-- gear items


deleteGearItem :: Text -> IO ()
deleteGearItem itemId = do
  --putStrLn "Delete not implemented"
  conn <- open dbName
  runBeamSqlite conn $ runDelete $ delete (_gear_items db) (\item -> _gearitemId item ==. val_ itemId)

updateGearItem :: GearItem -> IO ()
updateGearItem gearItem = do
  conn <- open dbName
  runBeamSqlite conn $ runUpdate $ save (_gear_items db) gearItem

putGearItem :: GearItem -> IO ()
putGearItem gearItem = do
  conn <- open dbName
  runBeamSqlite conn $ runInsert $ insert (_gear_items db) $ insertValues [ gearItem ]

getAllGearItems :: IO [GearItem]
getAllGearItems = do
  conn <- open dbName
  runBeamSqlite conn $ runSelectReturningList $ select $ all_ (_gear_items db)

getGearItemById :: Text -> IO (Maybe GearItem)
getGearItemById gearItemId = do
  conn <- open dbName
  let matchingItems = lookup_ (_gear_items db) (GearItemId gearItemId)
  runBeamSqlite conn $ runSelectReturningOne matchingItems

clearAllGearItems :: IO ()
clearAllGearItems = do
  conn <- open dbName
  runBeamSqlite conn $ runDelete $ delete (_gear_items db) (\item -> _gearitemId item /=. "")
