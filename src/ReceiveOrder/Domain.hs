{-# LANGUAGE TemplateHaskell #-}

module ReceiveOrder.Domain
  ( buildReceiveOrder
  , ByCid(..)
  , Quantity(..)
  , ReceiveOrder(..)
  , ReceiveOrderAttributes(..)
  , ReceiveOrderErrors(..)
  , ReceiveOrderItem(..)
  , ReceiveOrderItemAttributes(..)
  , ReceiveOrdersRequest(..)
  ) where

import Control.Monad
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.List
import qualified Data.Map.Strict as M
import Data.Time.Clock

data ReceiveOrdersRequest = ReceiveOrdersRequest { receive_orders :: ByCid ReceiveOrderAttributes }

data ReceiveOrderAttributes = ReceiveOrderAttributes
  { vendor_name         :: String
  , receive_order_items :: [ReceiveOrderItemAttributes]
  } deriving (Show, Eq)

data ReceiveOrderItemAttributes = ReceiveOrderItemAttributes
  { sku_id                          :: Integer
  , unit_quantity_value             :: Double
  , unit_of_measure_integration_key :: String
  } deriving (Show, Eq)

data ReceiveOrderErrors = ReceiveOrderErrors
  { full_messages :: [String]
  , errors :: M.Map String [String] 
  } deriving (Show, Eq)

type ByCid = M.Map String

data ReceiveOrder = ReceiveOrder
  { vendor             :: String
  , expectedDeliveryAt :: Maybe UTCTime
  , reference          :: Maybe String
  , receiveOrderItems  :: [ReceiveOrderItem]
  } deriving(Eq, Show)

data ReceiveOrderItem = ReceiveOrderItem
  { skuId    :: Integer
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
$(deriveJSON defaultOptions ''ReceiveOrdersRequest)
$(deriveJSON defaultOptions ''Quantity)

maxNumberOfReceiveOrderItems :: Int
maxNumberOfReceiveOrderItems = 100

buildReceiveOrder :: ReceiveOrderAttributes -> Either ReceiveOrderErrors ReceiveOrder
buildReceiveOrder = validateReceiveOrder . receiveOrderFromAttributes
  where

  receiveOrderFromAttributes :: ReceiveOrderAttributes -> ReceiveOrder
  receiveOrderFromAttributes attributes = ReceiveOrder {
    vendor             = vendor_name attributes,
    expectedDeliveryAt = Nothing,
    reference          = Nothing,
    receiveOrderItems  = buildReceiveOrderItem <$> receive_order_items attributes
  }

  buildReceiveOrderItem :: ReceiveOrderItemAttributes -> ReceiveOrderItem
  buildReceiveOrderItem itemAttributes = ReceiveOrderItem {
    skuId    = sku_id itemAttributes,
    quantity = Quantity {
      value         = unit_quantity_value itemAttributes,
      unitOfMeasure = unit_of_measure_integration_key itemAttributes
    }
  }

validateReceiveOrder :: ReceiveOrder -> Either ReceiveOrderErrors ReceiveOrder
validateReceiveOrder = validateNumberOfItems >=> validateUniqueSkusForItems

validateNumberOfItems :: ReceiveOrder -> Either ReceiveOrderErrors ReceiveOrder
validateNumberOfItems ro@ReceiveOrder { receiveOrderItems = items }
  | length items > maxNumberOfReceiveOrderItems =
    Left $ ReceiveOrderErrors {
      full_messages = [ "Can only have 100 order items per Receive Order" ],
      errors = M.singleton "base" [ "Can only have 100 order items per Receive Order" ]
    }
  | otherwise = Right ro

validateUniqueSkusForItems :: ReceiveOrder -> Either ReceiveOrderErrors ReceiveOrder
validateUniqueSkusForItems ro@ReceiveOrder { receiveOrderItems = items }
  | length uniqueSkus < length allSkus =
    Left $ ReceiveOrderErrors {
      full_messages = [ "Sku already exists for this Receive Order" ],
      errors = M.singleton "sku" [ "already exists for this Receive Order" ]
    }
  | otherwise = Right ro

  where

    allSkus :: [Integer]
    allSkus = skuId <$> items

    uniqueSkus :: [Integer]
    uniqueSkus = nub $ skuId <$> items
