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

massCreate :: (ByCid ReceiveOrderAttributes -> IO (Either (ByCid ReceiveOrderErrors) (ByCid ReceiveOrder)))
  -> ReceiveOrdersRequest
  -> Handler (ByCid ReceiveOrder)
massCreate createFn attributes = lift (createFn $ adaptParameters attributes) >>= either err return
  where
    err :: ByCid ReceiveOrderErrors -> Handler (ByCid ReceiveOrder)
    err msg = throwError $ err503 { errBody = encode msg }

    adaptParameters :: ReceiveOrdersRequest -> ByCid ReceiveOrderAttributes
    adaptParameters = receive_orders
