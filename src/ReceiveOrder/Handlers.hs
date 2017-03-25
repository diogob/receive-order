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

massCreate :: (AttributesByCid -> IO (ByCid (Either ReceiveOrderErrors ReceiveOrder))) -> AttributesByCid -> Handler (ByCid (Either ReceiveOrderErrors ReceiveOrder))
massCreate createFn attributes = lift (createFn attributes)
