{-# LANGUAGE TemplateHaskell #-}

module Domain
  ( buildReceiveOrder
  , Quantity(..)
  , ReceiveOrder(..)
  , ReceiveOrderItem(..)
  , ReceiveOrderAttributes(..)
  , ReceiveOrderItemAttributes(..)
  ) where

import Data.Time.Clock
import Data.Aeson.TH (deriveJSON, defaultOptions)

data ReceiveOrderAttributes = ReceiveOrderAttributes
  { vendorName                  :: String
  , receiveOrderItemsAttributes :: [ReceiveOrderItemAttributes]
  } deriving (Show, Eq)

data ReceiveOrderItemAttributes = ReceiveOrderItemAttributes
  { skuCode                     :: String
  , unitQuantityValue           :: Double
  , unitOfMeasureIntegrationKey :: String
  } deriving (Show, Eq)

data ReceiveOrder = ReceiveOrder
  { vendor             :: String
  , expectedDeliveryAt :: Maybe UTCTime
  , reference          :: Maybe String
  , receiveOrderItems  :: [ReceiveOrderItem]
  } deriving(Eq, Show)

data ReceiveOrderItem = ReceiveOrderItem
  { sku      :: String
  , quantity :: Quantity
  } deriving(Eq, Show)

data Quantity = Quantity
  { value         :: Double
  , unitOfMeasure :: String
  } deriving(Eq, Show)

$(deriveJSON defaultOptions ''ReceiveOrderAttributes)
$(deriveJSON defaultOptions ''ReceiveOrderItemAttributes)
$(deriveJSON defaultOptions ''ReceiveOrder)
$(deriveJSON defaultOptions ''ReceiveOrderItem)
$(deriveJSON defaultOptions ''Quantity)

buildReceiveOrder :: ReceiveOrderAttributes -> ReceiveOrder
buildReceiveOrder attributes = ReceiveOrder {
  vendor             = vendorName attributes,
  expectedDeliveryAt = Nothing,
  reference          = Nothing,
  receiveOrderItems  = map buildReceiveOrderItem $ receiveOrderItemsAttributes attributes
  } where

  buildReceiveOrderItem :: ReceiveOrderItemAttributes -> ReceiveOrderItem
  buildReceiveOrderItem itemAttributes = ReceiveOrderItem {
    sku      = skuCode itemAttributes,
    quantity = Quantity {
      value         = unitQuantityValue itemAttributes,
      unitOfMeasure = unitOfMeasureIntegrationKey itemAttributes
    }
  }
