module ReceiveOrder.Handlers
  ( massCreate
  ) where

import Servant
import Data.String.Conversions

import ReceiveOrder.Database
import Domain

massCreate :: ([ReceiveOrderAttributes] -> Either Error [ReceiveOrder]) -> [ReceiveOrderAttributes] -> Handler [ReceiveOrder]
massCreate createFn attributes = either err return (createFn attributes)
  where
    err :: Error -> Handler [ReceiveOrder]
    err msg = throwError $ err503 { errBody = (cs . show) msg }
