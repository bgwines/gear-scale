{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module ServerAPI
  ( LimitedAPI
  , FullAPI
  , limitedApi
  , fullApi
  ) where

import Data.Aeson ( ToJSON )
import qualified Data.Text as T
import Data.Proxy ( Proxy(..) )
import GHC.Generics ( Generic )
import Servant
  ( type (:<|>)(..)
  , type (:>)
  , Get
  , JSON
  , Post
  , Raw
  , ReqBody
  , Required
  , Strict
  , QueryParam )
import qualified Servant.API.ContentTypes
import qualified DB

type LimitedAPI
    =    "putGearItem"     :> ReqBody '[JSON] DB.GearItem
                           :> Post '[JSON] T.Text
    :<|> "searchGearItems" :> QueryParam "q" T.Text
                           :> Get '[JSON] [DB.GearItem]

type FullAPI = LimitedAPI :<|> Raw

limitedApi :: Proxy LimitedAPI
limitedApi = Proxy

fullApi :: Proxy FullAPI
fullApi = Proxy
