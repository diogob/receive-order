{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module ReceiveOrder.Api
    ( server
    , api
    ) where

import Servant
import Hasql.Pool (Pool)

import ReceiveOrder.Handlers
import ReceiveOrder.Database

type API = "users" :> Post '[JSON] [User]

api :: Proxy API
api = Proxy

server :: Pool -> Server API
server = massCreate . createUser

