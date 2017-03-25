module ReceiveOrder.Database where

import Domain
import Data.Text
import Hasql.Pool

newtype Error = Error Text deriving (Show)

createReceiveOrders :: Pool -> AttributesByCid -> IO (Either ReceiveOrderErrors ReceiveOrdersByCid)
createReceiveOrders _ = return . sequence . fmap buildReceiveOrder
