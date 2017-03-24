module ReceiveOrder.Handlers
  ( massCreate
  ) where

import Servant
import Data.Map.Strict as M
import Data.String.Conversions
import Control.Monad.Trans.Class (lift)

import ReceiveOrder.Database
import Domain

massCreate :: ((M.Map String ReceiveOrderAttributes) -> IO (Either ReceiveOrderErrors (M.Map String ReceiveOrder))) -> M.Map String ReceiveOrderAttributes -> Handler (M.Map String ReceiveOrder)
massCreate createFn attributes = lift (createFn attributes) >>= either err return
  where
    err :: ReceiveOrderErrors -> Handler (M.Map String ReceiveOrder)
    err msg = throwError $ err503 { errBody = (cs . show) msg }
