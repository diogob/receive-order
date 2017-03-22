module ReceiveOrder.Database where

import Domain
import Data.Text
import Hasql.Pool

newtype Error = Error Text deriving (Show)

createUser :: Pool -> [ReceiveOrderAttributes] -> Either Error [ReceiveOrder]
createUser _ = Right . fmap buildReceiveOrder
