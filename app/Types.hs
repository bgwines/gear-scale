{-# LANGUAGE DeriveGeneric #-}

module Types
  (
  ) where

import Data.Aeson ( ToJSON )
import Data.Text as T (Text)
import GHC.Generics ( Generic )


--data GearSelection = GearSelection
--  { item :: GearItem
--  , selected :: Bool
--  , isPersonalOverride :: Maybe Bool
--  , inPack :: Bool
--  , isOptional :: Bool
--  , quantity :: Int
--  } deriving Generic

--instance ToJSON GearSelection
