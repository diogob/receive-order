{-# LANGUAGE TemplateHaskell #-}

module Domain
  ( buildReceiveOrder
  , Quantity(..)
  , ReceiveOrder(..)
  , ReceiveOrderErrors(..)
  , ReceiveOrderItem(..)
  , ReceiveOrderAttributes(..)
  , ReceiveOrderItemAttributes(..)
  ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)
import qualified Data.Map.Strict as M
import Data.Time.Clock

data ReceiveOrderAttributes = ReceiveOrderAttributes
  { vendorName                  :: String
  , receiveOrderItemsAttributes :: [ReceiveOrderItemAttributes]
  } deriving (Show, Eq)

data ReceiveOrderItemAttributes = ReceiveOrderItemAttributes
  { skuCode                     :: String
  , unitQuantityValue           :: Double
  , unitOfMeasureIntegrationKey :: String
  } deriving (Show, Eq)

data ReceiveOrderErrors = ReceiveOrderErrors
  { fullMessages :: [String]
  , errors :: M.Map String [String] 
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
$(deriveJSON defaultOptions ''ReceiveOrderErrors)
$(deriveJSON defaultOptions ''ReceiveOrder)
$(deriveJSON defaultOptions ''ReceiveOrderItem)
$(deriveJSON defaultOptions ''Quantity)

maxNumberOfReceiveOrderItems :: Int
maxNumberOfReceiveOrderItems = 100

buildReceiveOrder :: ReceiveOrderAttributes -> Either ReceiveOrderErrors ReceiveOrder
buildReceiveOrder = validateReceiveOrder . receiveOrderFromAttributes
  where

  receiveOrderFromAttributes :: ReceiveOrderAttributes -> ReceiveOrder
  receiveOrderFromAttributes attributes = ReceiveOrder {
    vendor             = vendorName attributes,
    expectedDeliveryAt = Nothing,
    reference          = Nothing,
    receiveOrderItems  = map buildReceiveOrderItem $ receiveOrderItemsAttributes attributes
  }

  buildReceiveOrderItem :: ReceiveOrderItemAttributes -> ReceiveOrderItem
  buildReceiveOrderItem itemAttributes = ReceiveOrderItem {
    sku      = skuCode itemAttributes,
    quantity = Quantity {
      value         = unitQuantityValue itemAttributes,
      unitOfMeasure = unitOfMeasureIntegrationKey itemAttributes
    }
  }

validateReceiveOrder :: ReceiveOrder -> Either ReceiveOrderErrors ReceiveOrder
validateReceiveOrder = validateNumberOfItems
  where

  validateNumberOfItems :: ReceiveOrder -> Either ReceiveOrderErrors ReceiveOrder
  validateNumberOfItems ro@ReceiveOrder { receiveOrderItems = items }
    | length items > maxNumberOfReceiveOrderItems =
      Left $ ReceiveOrderErrors {
        fullMessages = [ "can only have 100 order items per receive order" ],
        errors = M.singleton "Receive Order" [ "can only have 100 order items per receive order" ]
      }
    | otherwise = Right ro
