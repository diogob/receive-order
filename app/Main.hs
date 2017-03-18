module Main where

import           ReceiveOrder.Api
import           ReceiveOrder.Config

main :: IO ()
main = do
  conf <- readOptions
  startApp $ port conf
