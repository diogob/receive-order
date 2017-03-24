module ReceiveOrder.Database where

import Domain
import Data.Text
import Hasql.Pool

newtype Error = Error Text deriving (Show)

createUser :: Pool -> AttributesByCid -> IO (Either ReceiveOrderErrors ReceiveOrdersByCid)
createUser _ = return . sequence . fmap buildReceiveOrder
