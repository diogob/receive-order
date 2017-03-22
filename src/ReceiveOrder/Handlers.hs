module ReceiveOrder.Handlers
  ( massCreate
  ) where

import Servant
import Data.String.Conversions
import Control.Monad.Trans.Class (lift)

import ReceiveOrder.Database
import Domain

massCreate :: ([ReceiveOrderAttributes] -> IO (Either Error [ReceiveOrder])) -> [ReceiveOrderAttributes] -> Handler [ReceiveOrder]
massCreate createFn attributes = lift (createFn attributes) >>= either err return
  where
    err :: Error -> Handler [ReceiveOrder]
    err msg = throwError $ err503 { errBody = (cs . show) msg }
