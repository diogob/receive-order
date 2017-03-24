module ReceiveOrder.Database where

import Domain
import qualified Data.Map.Strict as M
import Data.Text
import Hasql.Pool

newtype Error = Error Text deriving (Show)

createUser :: Pool -> M.Map String ReceiveOrderAttributes -> IO (Either ReceiveOrderErrors (M.Map String ReceiveOrder))
createUser _ = return . sequence . fmap buildReceiveOrder
