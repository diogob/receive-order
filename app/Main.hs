module Main where

import           ReceiveOrder.Api
import           ReceiveOrder.Config

main :: IO ()
main = readOptions >>= startApp