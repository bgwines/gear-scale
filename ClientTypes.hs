{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module ClientTypes
  ( GearKind(..)
  , GearItem(..)
  ) where

import Data.Aeson.TH ( defaultOptions, deriveJSON )

import qualified Data.Text
import qualified GHC.Generics


data GearKind = Base | Technical | Clothing | Electronic | Nutrition
  deriving (Eq, Show, Read)

data GearItem = GearItem
    { itemId        :: Data.Text.Text
    , name          :: Data.Text.Text
    , isPersonal    :: Bool
    , oz            :: Double
    , kind          :: GearKind
    , creatorUserId :: Data.Text.Text }
    deriving (Eq, Show)

$(deriveJSON defaultOptions ''GearKind)
$(deriveJSON defaultOptions ''GearItem)
