module Main where

import           Data.String.Conversions
import           Hasql.Pool               (acquire)
import           Network.Wai.Handler.Warp

import           Config
import           ReceiveOrder.Api

main :: IO ()
main = readOptions >>= startApp

startApp :: AppConfig -> IO ()
startApp conf = do
  putStrLn $ "Listening on port " <> show portNumber
  pool <- acquire (10, 10, cs $ pgConnection conf)
  run portNumber $ app pool
  where
    portNumber = port conf
    app = serve api . server
