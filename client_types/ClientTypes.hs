{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module ClientTypes
  ( GearItem(..)
  , GearKind
  ) where

import qualified Data.Text
import qualified GHC.Generics
import Data.Aeson ( ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions )

data GearItem = GearItem
    { itemId        :: Data.Text.Text
    , name          :: Data.Text.Text
    , isPersonal    :: Bool
    , oz            :: Double
    , kind          :: GearKind
    , creatorUserId :: Data.Text.Text }
    deriving (GHC.Generics.Generic, Eq, Show, FromJSON, ToJSON)


data GearKind = Base | Technical | Clothing | Electronic | Nutrition
  deriving (GHC.Generics.Generic, Eq, Show, Read, FromJSON, ToJSON)
