module Domain
  ( buildReceiveOrder
  , Quantity(..)
  , ReceiveOrder(..)
  , ReceiveOrderItem(..)
  , ReceiveOrderAttributes(..)
  , ReceiveOrderItemAttributes(..)
  ) where

import Data.Time.Clock

data ReceiveOrderAttributes = ReceiveOrderAttributes
  { vendorName                  :: String
  , receiveOrderItemsAttributes :: [ReceiveOrderItemAttributes]
  }

data ReceiveOrderItemAttributes = ReceiveOrderItemAttributes
  { skuCode                     :: String
  , unitQuantityValue           :: Double
  , unitOfMeasureIntegrationKey :: String
  }

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

buildReceiveOrder :: ReceiveOrderAttributes -> ReceiveOrder
buildReceiveOrder attributes = ReceiveOrder {
  vendor             = vendorName attributes,
  expectedDeliveryAt = Nothing,
  reference          = Nothing,
  receiveOrderItems  = map buildReceiveOrderItem $ receiveOrderItemsAttributes attributes
  } where

  buildReceiveOrderItem :: ReceiveOrderItemAttributes -> ReceiveOrderItem
  buildReceiveOrderItem attributes = ReceiveOrderItem {
    sku      = skuCode attributes,
    quantity = Quantity {
      value         = unitQuantityValue attributes,
      unitOfMeasure = unitOfMeasureIntegrationKey attributes
    }
  }
