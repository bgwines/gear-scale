{-# LANGUAGE DeriveGeneric #-}

module Types
  ( GearKind(..)
  , GearItem(..)
  , GearSelection(..)
  , Search(..)
  , Book(..)
  , Point(..)
  ) where

import Data.Aeson ( ToJSON )
import Data.Text as T (Text)
import GHC.Generics ( Generic )

data GearKind = Base | Technical | Clothing | Electronic | Nutrition
  deriving Generic

instance ToJSON GearKind


data GearItem = GearItem
  { name :: Text
  , kind :: GearKind
  , isPersonal :: Bool
  , oz :: Double
  , id :: Text
  } deriving Generic

instance ToJSON GearItem


data GearSelection = GearSelection
  { item :: GearItem
  , selected :: Bool
  , isPersonalOverride :: Maybe Bool
  , inPack :: Bool
  , isOptional :: Bool
  , quantity :: Int
  } deriving Generic

instance ToJSON GearSelection

data Point = Point
  { x :: Double
  , y :: Double
  } deriving Generic

instance ToJSON Point


data Search a = Search
  { query   :: T.Text
  , results :: [a]
  } deriving Generic

instance ToJSON a => ToJSON (Search a)

data Book = Book
  { author :: T.Text
  , title  :: T.Text
  , year   :: Int
  } deriving Generic

instance ToJSON Book
