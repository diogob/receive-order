{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module ReceiveOrder.Api
    ( server
    , api
    , serve
    ) where

import Servant
import Hasql.Pool (Pool)

import Domain
import ReceiveOrder.Handlers
import ReceiveOrder.Database

type API = "receive_orders" :> ReqBody '[JSON] [ReceiveOrderAttributes] :> Post '[JSON] [ReceiveOrder]

api :: Proxy API
api = Proxy

server :: Pool -> Server API
server = massCreate . createUser
