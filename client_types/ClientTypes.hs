{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module ClientTypes
  ( GearKind(..)
  , GearItem(..)
  ) where

import Data.Aeson.TH
import Data.Aeson.TypeScript.TH

import Data.Proxy
import qualified Data.Text
import qualified GHC.Generics
--import Data.Aeson ( ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions )

data GearKind = Base | Technical | Clothing | Electronic | Nutrition
  deriving (Eq, Show, Read)
  --deriving (GHC.Generics.Generic, Eq, Show, Read, FromJSON, ToJSON)


data GearItem = GearItem
    { itemId        :: Data.Text.Text
    , name          :: Data.Text.Text
    , isPersonal    :: Bool
    , oz            :: Double
    , kind          :: GearKind
    , creatorUserId :: Data.Text.Text }
    deriving (Eq, Show)
    --deriving (GHC.Generics.Generic, Eq, Show, FromJSON, ToJSON)

--instance HasJSONOptions GearKind where getJSONOptions _ = defaultOptions
--instance HasJSONOptions GearItem where getJSONOptions _ = defaultOptions


$(deriveJSON defaultOptions ''GearKind)
$(deriveJSON defaultOptions ''GearItem)
