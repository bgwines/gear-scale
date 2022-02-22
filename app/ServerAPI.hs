{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

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
import qualified ClientTypes

type LimitedAPI
    =    "putGearItem"     :> ReqBody '[JSON] ClientTypes.GearItem
                           :> Post '[JSON] T.Text
    :<|> "searchGearItems" :> QueryParam "q" T.Text
                           :> Get '[JSON] [ClientTypes.GearItem]

type FullAPI = LimitedAPI :<|> Raw

limitedApi :: Proxy LimitedAPI
limitedApi = Proxy

fullApi :: Proxy FullAPI
fullApi = Proxy
