module Main where

import           ReceiveOrder.Api
import           Config
import Hasql.Pool (Pool, acquire)
import Servant
import Data.String.Conversions
import Network.Wai.Handler.Warp

main :: IO ()
main = readOptions >>= startApp

startApp :: AppConfig -> IO ()
startApp conf = do
  putStrLn $ "Listening on port " <> show portNumber
  pool <- acquire (10, 10, cs $ pgConnection conf)
  run portNumber $ app pool
  where
    portNumber = port conf

app :: Pool -> Application
app = serve api . server
