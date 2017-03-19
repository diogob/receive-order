{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module ReceiveOrder.Api
    ( startApp
    , app
    ) where

import Data.Monoid
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Hasql.Pool (Pool)
import qualified Hasql.Pool as P
import Data.String.Conversions (cs)

import ReceiveOrder.Handlers
import ReceiveOrder.Config
import ReceiveOrder.Database

type API = "users" :> Post '[JSON] [User]

startApp :: AppConfig -> IO ()
startApp conf = do
  putStrLn $ "Listening on port " <> show portNumber
  pool <- P.acquire (10, 10, cs $ pgConnection conf)
  run portNumber $ app pool
  where
    portNumber = port conf

app :: Pool -> Application
app = serve api . server

api :: Proxy API
api = Proxy

server :: Pool -> Server API
server = massCreate . createUser

