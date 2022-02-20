{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DB
  ( UserT
  , User
  , UserId
  , Db
  , db
  , putUser
  , getAllUsers
  , putGearItem
  , getAllGearItems
  , GearItemT
  , GearItem
  , GearItemId
  ) where

import Database.Beam
    ( Generic,
      Identity,
      insert,
      insertValues,
      runInsert,
      runSelectReturningList,
      select,
      all_,
      defaultDbSettings,
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

-----------
-- UserT --
-----------

data UserT f
    = User
    { _userId        :: Columnar f Text
    , _userFirstName :: Columnar f Text }
    deriving Generic
instance Beamable UserT

type User = UserT Identity
deriving instance Show User
deriving instance Eq User

type UserId = PrimaryKey UserT Identity

instance Table UserT where
   data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic, Beamable)
   primaryKey = UserId . _userId

--------------
-- GearKind --
--------------

data GearKind = Base | Technical | Clothing | Electronic | Nutrition
  deriving (Generic, Eq, Show, Read)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be GearKind where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite GearKind where
  fromBackendRow = read . unpack <$> fromBackendRow

instance FromJSON GearKind
instance ToJSON GearKind where
    toEncoding = genericToEncoding defaultOptions

---------------
-- GearItemT --
---------------

data GearItemT f
    = GearItem
    { _gearItemId            :: Columnar f Text
    , _gearItemName          :: Columnar f Text
    , _gearItemIsPersonal    :: Columnar f Bool
    , _gearItemOz            :: Columnar f Double
    , _gearItemKind          :: Columnar f GearKind
    , _gearItemCreatorUserId :: Columnar f Text }
    deriving Generic
instance Beamable GearItemT

type GearItem = GearItemT Identity
deriving instance Show GearItem
deriving instance Eq GearItem
instance ToJSON GearItem where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON GearItem

    -- No need to provide a parseJSON implementation
type GearItemId = PrimaryKey GearItemT Identity

instance Table GearItemT where
   data PrimaryKey GearItemT f = GearItemId (Columnar f Text) deriving (Generic, Beamable)
   primaryKey = GearItemId . _gearItemId

--------
-- DB --
--------

data Db f = Db
  { _users :: f (TableEntity UserT)
  , _gearItems :: f (TableEntity GearItemT) }
  deriving (Generic, Database be)

db :: DatabaseSettings be Db
db = defaultDbSettings

dbName :: String
dbName = "gear_scale.db"

------------
-- DB API --
------------

putUser :: User -> IO ()
putUser user = do
  conn <- open dbName
  runBeamSqlite conn $ runInsert $ insert (_users db) $ insertValues [ user ]

getAllUsers :: IO [UserT Identity]
getAllUsers = do
  conn <- open dbName
  let allUsers = all_ (_users db)

  runBeamSqlite conn $ runSelectReturningList $ select allUsers

putGearItem :: GearItem -> IO ()
putGearItem gearItem = do
  conn <- open dbName -- TODO resource pool
  runBeamSqlite conn $ runInsert $ insert (_gearItems db) $ insertValues [ gearItem ]

getAllGearItems :: IO [GearItemT Identity]
getAllGearItems = do
  conn <- open dbName
  let allItems = all_ (_gearItems db)

  runBeamSqlite conn $ runSelectReturningList $ select allItems
