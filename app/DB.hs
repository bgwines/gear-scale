{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module DB (UserT, User, UserId, Db, db, put, get) where

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

data Db f = Db
  { _users :: f (TableEntity UserT) }
  deriving (Generic, Database be)

db :: DatabaseSettings be Db
db = defaultDbSettings

dbName :: String
dbName = "gear_scale.db"

put :: IO ()
put = do
  conn <- open dbName
  runBeamSqliteDebug putStrLn conn $ runInsert $ insert (_users db) $ insertValues [ User "b4cc344d25a2efe540adbf2678e2304c" "James"
                 , User "82b054bd83ffad9b6cf8bdb98ce3cc2f" "Betty"
                 , User "332532dcfaa1cbf61e2a266bd723612c" "Sam"  ]

get :: IO [UserT Identity]
get = do
  conn <- open dbName
  let allUsers = all_ (_users db)

  runBeamSqliteDebug putStrLn conn $ runSelectReturningList $ select allUsers
