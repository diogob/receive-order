{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings   #-}
module ReceiveOrder.Api
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import ReceiveOrder.Handlers

type API = "users" :> Post '[JSON] [User]

startApp :: Port -> IO ()
startApp = flip run app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = massCreate
