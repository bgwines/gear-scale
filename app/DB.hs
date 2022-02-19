{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

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
import Database.Beam.Sqlite ( runBeamSqliteDebug )
import Database.SQLite.Simple ( open )
import Data.Text (Text)
import Data.Aeson ( ToJSON )

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

---------------
-- GearItemT --
---------------

data GearItemT f
    = GearItem
    { _gearItemId            :: Columnar f Text
    , _gearItemName          :: Columnar f Text
    , _gearItemIsPersonal    :: Columnar f Bool
    , _gearItemOz            :: Columnar f Double
    , _gearItemCreatorUserId :: Columnar f Text }
    deriving Generic
instance Beamable GearItemT

type GearItem = GearItemT Identity
deriving instance Show GearItem
deriving instance Eq GearItem
instance ToJSON GearItem

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
  runBeamSqliteDebug putStrLn conn $ runInsert $ insert (_users db) $ insertValues [ user ]

getAllUsers :: IO [UserT Identity]
getAllUsers = do
  conn <- open dbName
  let allUsers = all_ (_users db)

  runBeamSqliteDebug putStrLn conn $ runSelectReturningList $ select allUsers

putGearItem :: GearItem -> IO ()
putGearItem user = do
  conn <- open dbName
  runBeamSqliteDebug putStrLn conn $ runInsert $ insert (_gearItems db) $ insertValues [ user ]

getAllGearItems :: IO [GearItemT Identity]
getAllGearItems = do
  conn <- open dbName
  let allItems = all_ (_gearItems db)

  runBeamSqliteDebug putStrLn conn $ runSelectReturningList $ select allItems
