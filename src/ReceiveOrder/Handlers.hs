{-|
Module      : ReceiveOrder.Handlers
Description : API Handlers.
This module provides functions to convert the results from the domain to a type that the API can interact with.
It should contain most knowledge about HTTP types, aeson and servant.
Knowledge about domain and database stuf comes from other modules.
-}
module ReceiveOrder.Handlers
  ( massCreate
  , ReceiveOrdersRequest(..)
  ) where

import Control.Monad.Trans.Class (lift)
import Data.String.Conversions
import ReceiveOrder.Domain
import Network.HTTP.Types
import Data.Aeson (encode)
import Servant

{-
  It takes a function that inserts the ReceiveOrders in the database
  and returns a function that Handles POST receive_orders
-}
massCreate :: (ByCid ReceiveOrderAttributes -> IO (Either (ByCid ReceiveOrderErrors) (ByCid ReceiveOrder)))
  -> ReceiveOrdersRequest
  -> Handler (ByCid ReceiveOrder)
massCreate createFn attributes = lift (createFn $ adaptParameters attributes) >>= either err return
  where
    err :: ByCid ReceiveOrderErrors -> Handler (ByCid ReceiveOrder)
    err msg = throwError $ err503 { errBody = encode msg }

    adaptParameters :: ReceiveOrdersRequest -> ByCid ReceiveOrderAttributes
    adaptParameters = receive_orders
