module ReceiveOrder.Handlers
  ( massCreate
  ) where

import Servant
import Data.String.Conversions
import Control.Monad.Trans.Class (lift)

import ReceiveOrder.Database
import Domain

massCreate :: (AttributesByCid -> IO (Either ReceiveOrderErrors ReceiveOrdersByCid)) -> AttributesByCid -> Handler ReceiveOrdersByCid
massCreate createFn attributes = lift (createFn attributes) >>= either err return
  where
    err :: ReceiveOrderErrors -> Handler ReceiveOrdersByCid
    err msg = throwError $ err503 { errBody = (cs . show) msg }
