{-# LANGUAGE DeriveGeneric #-}

module Types
  ( GearKind
  , GearItem
  , GearSelection
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
