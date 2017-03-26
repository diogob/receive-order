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
import Data.Function
import Data.List
import Data.Ord
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

-- Exercise 6: Wire it all together.
buildReceiveOrder :: ReceiveOrderAttributes -> Either ReceiveOrderErrors ReceiveOrder
buildReceiveOrder = validateReceiveOrder . rollUpQuantities . receiveOrderFromAttributes

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

-- Exercise 5: Implement this roll-up function.
rollUpQuantities :: ReceiveOrder -> ReceiveOrder
rollUpQuantities ro@ReceiveOrder { receiveOrderItems = items } =
  ro { receiveOrderItems = (fmap rollUp . groupBySku . sortBySku) $ items }
  where

  sortBySku :: [ReceiveOrderItem] -> [ReceiveOrderItem]
  sortBySku = sortBy (comparing skuId)

  groupBySku :: [ReceiveOrderItem] -> [[ReceiveOrderItem]]
  groupBySku = groupBy ((==) `on` skuId)

  rollUp :: [ReceiveOrderItem] -> ReceiveOrderItem
  rollUp []           = undefined
  rollUp (item:items) = item {
    quantity = Quantity {
      unitOfMeasure = unitOfMeasure . quantity $ item,
      value         = (value . quantity) item + (sum $ fmap (value . quantity) items)
    }
  }

-- Exercise 4: Abstract the commonality of these two validations into a higher-order function.
validate :: (ReceiveOrder -> Bool) -> [String] -> M.Map String [String] -> ReceiveOrder -> Either ReceiveOrderErrors ReceiveOrder
validate validator fullMessages errors receiveOrder
  | not $ validator receiveOrder =
    Left $ ReceiveOrderErrors {
      full_messages = fullMessages,
      errors = errors
    }
  | otherwise = Right receiveOrder

-- Exercise 3: Compose the two validations together.
validateReceiveOrder :: ReceiveOrder -> Either ReceiveOrderErrors ReceiveOrder
validateReceiveOrder = validateNumberOfItems >=> validatePositiveUnitQuantities

-- Exercise 1: Implement this validation.
validateNumberOfItems :: ReceiveOrder -> Either ReceiveOrderErrors ReceiveOrder
validateNumberOfItems = validate ((< maxNumberOfReceiveOrderItems) . length . receiveOrderItems)
  [ "Can only have 100 order items per Receive Order" ]
  (M.singleton "base" [ "Can only have 100 order items per Receive Order" ])

-- Exercise 2: Implement this validation too.
validatePositiveUnitQuantities :: ReceiveOrder -> Either ReceiveOrderErrors ReceiveOrder
validatePositiveUnitQuantities = validate (all ((> 0) . value . quantity) . receiveOrderItems)
  [ "Receive Order Item unit quantity must be greater than or equal to 0" ]
  (M.singleton "receive_order_item.unit_quantity" [ "unit quantity must be greater than or equal to 0" ])
