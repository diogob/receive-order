{-# LANGUAGE TemplateHaskell #-}

module ReceiveOrder.Handlers
  ( massCreate
  , ReceiveOrdersRequest(..)
  ) where

import Control.Monad.Trans.Class (lift)

import Data.String.Conversions

import Domain

import Network.HTTP.Types

import Servant

massCreate :: (ByCid ReceiveOrderAttributes -> IO (ByCid (Either ReceiveOrderErrors ReceiveOrder)))
  -> ReceiveOrdersRequest
  -> Handler (ByCid (Either ReceiveOrderErrors ReceiveOrder))
massCreate createFn attributes = lift (createFn $ adaptParameters attributes)
  where
  
  adaptParameters :: ReceiveOrdersRequest -> ByCid ReceiveOrderAttributes
  adaptParameters = receive_orders
