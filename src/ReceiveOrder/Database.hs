module ReceiveOrder.Database where

import Data.Text
import qualified Data.Map as M
import Hasql.Pool
import ReceiveOrder.Domain

createReceiveOrders :: Pool -> ByCid ReceiveOrderAttributes -> IO (Either (ByCid ReceiveOrderErrors) (ByCid ReceiveOrder))
createReceiveOrders _  = return . tupleToEither . M.mapEither buildReceiveOrder
  where
    tupleToEither t
      | hasError t = Left $ fst t
      | otherwise = Right $ snd t
    hasError = not . M.null . fst
