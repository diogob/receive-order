module Main where

import Data.Monoid
import qualified Hasql.Pool as P
import Data.String.Conversions (cs)

import           ReceiveOrder.Api
import           ReceiveOrder.Config

main :: IO ()
main = do
  conf <- readOptions
  putStrLn $ "Listening on port " <> show (port conf)
  pool <- P.acquire (10, 10, cs $ pgConnection conf)
  startApp $ port conf
