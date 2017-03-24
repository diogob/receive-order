module ReceiveOrder.Database where

import Domain
import Data.Text
import Hasql.Pool

newtype Error = Error Text deriving (Show)

createUser :: Pool -> [ReceiveOrderAttributes] -> IO (Either ReceiveOrderErrors [ReceiveOrder])
createUser _ = return . sequence . fmap buildReceiveOrder
