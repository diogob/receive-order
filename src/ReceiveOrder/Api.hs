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
import Data.Map.Strict as M
import ReceiveOrder.Handlers
import ReceiveOrder.Database

type API = "receive_orders" :> ReqBody '[JSON] (M.Map String ReceiveOrderAttributes) :> Post '[JSON] (M.Map String ReceiveOrder)

api :: Proxy API
api = Proxy

server :: Pool -> Server API
server = massCreate . createUser
