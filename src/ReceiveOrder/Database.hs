{-|
Module      : ReceiveOrder.Database
Description : Database manipulation.
This module provides functions that, given a database connection pool,
manipulate database tables.
-}

module ReceiveOrder.Database where

import Data.Text
import qualified Data.Map as M
import Hasql.Pool
import ReceiveOrder.Domain

{-
  Given a database connection pool returns a function that
  That converts ReceiveOrderAttributes to a Map of persisted ReceiveOrder (or a Map of errors for receive orders)
-}

createReceiveOrders :: Pool -> ByCid ReceiveOrderAttributes -> IO (Either (ByCid ReceiveOrderErrors) (ByCid ReceiveOrder))
createReceiveOrders _  = return . tupleToEither . M.mapEither buildReceiveOrder
  where
    tupleToEither t
      | hasError t = Left $ fst t
      | otherwise = Right $ snd t
    hasError = not . M.null . fst
