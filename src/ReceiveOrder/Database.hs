module ReceiveOrder.Database where

import Domain
import Data.Text
import Hasql.Pool

newtype Error = Error Text deriving (Show)

createReceiveOrders :: Pool -> ByCid ReceiveOrderAttributes -> IO (ByCid (Either ReceiveOrderErrors ReceiveOrder))
createReceiveOrders _ = return . fmap buildReceiveOrder
