module ReceiveOrder.Handlers
  ( massCreate
  ) where

import Control.Monad.Trans.Class (lift)

import Data.Aeson (toJSON)
import Data.Aeson.Text
import Data.String.Conversions

import Domain

import Network.HTTP.Types

import ReceiveOrder.Database

import Servant

massCreate :: (AttributesByCid -> IO (Either ReceiveOrderErrors ReceiveOrdersByCid)) -> AttributesByCid -> Handler ReceiveOrdersByCid
massCreate createFn attributes = lift (createFn attributes) >>= either err return
  where
    err :: ReceiveOrderErrors -> Handler ReceiveOrdersByCid
    err msg = throwError $ err503 {
      errBody = (cs . encodeToLazyText) msg,
      errHeaders = [(hContentType, "application/json")]
    }
