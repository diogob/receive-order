module ReceiveOrder.Database where

import Domain
import Data.Text
import Hasql.Pool

createReceiveOrders :: Pool -> ByCid ReceiveOrderAttributes -> IO (ByCid (Either ReceiveOrderErrors ReceiveOrder))
createReceiveOrders _ = return . fmap buildReceiveOrder
