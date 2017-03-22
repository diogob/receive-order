module ReceiveOrder.Database where

import Domain
import Data.Text
import Hasql.Pool

newtype Error = Error Text deriving (Show)

createUser :: Pool -> [ReceiveOrderAttributes] -> IO (Either Error [ReceiveOrder])
createUser _ = return . Right . fmap buildReceiveOrder
