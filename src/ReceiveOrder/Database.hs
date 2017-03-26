module ReceiveOrder.Database where

import Data.Text
import Hasql.Pool
import ReceiveOrder.Domain

createReceiveOrders :: Pool -> ByCid ReceiveOrderAttributes -> IO (ByCid (Either ReceiveOrderErrors ReceiveOrder))
createReceiveOrders _ = return . fmap buildReceiveOrder
