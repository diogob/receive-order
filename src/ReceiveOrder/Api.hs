{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module ReceiveOrder.Api
    ( server
    , api
    , serve
    ) where

import Servant
import Hasql.Pool (Pool)

import Data.Aeson
import Domain
import ReceiveOrder.Handlers
import ReceiveOrder.Database

type API = "receive_orders" :> ReqBody '[JSON] (ByCid ReceiveOrderAttributes) :> Post '[JSON] ReceiveOrdersResponse

newtype ReceiveOrdersResponse = ReceiveOrdersResponse (ByCid (Either ReceiveOrderErrors ReceiveOrder))

instance ToJSON ReceiveOrdersResponse where
  toJSON (ReceiveOrdersResponse r) = toJSON $ fmap (either toJSON toJSON) r

api :: Proxy API
api = Proxy

server :: Pool -> Server API
server = ((fmap . fmap) ReceiveOrdersResponse) . massCreate . createReceiveOrders
