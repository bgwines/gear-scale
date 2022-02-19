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
  , Raw
  , QueryParam )
import qualified Types

type LimitedAPI
  = "point" :> Get '[JSON] Types.Point
    :<|> "points" :> QueryParam "q" T.Text :> Get '[JSON] (Types.Search Types.Point)

type FullAPI = LimitedAPI :<|> Raw

limitedApi :: Proxy LimitedAPI
limitedApi = Proxy

fullApi :: Proxy FullAPI
fullApi = Proxy
