module ReceiveOrder.Domain.ReceiveOrderBuilder(buildReceiveOrder) where

import ReceiveOrder.Domain.Types

import Control.Monad

import Data.Function
import Data.List
import Data.Ord
import qualified Data.Map.Strict as M

maxNumberOfReceiveOrderItems :: Int
maxNumberOfReceiveOrderItems = 100

-- Exercise 1: Wire it all together.
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

-- Exercise 6: Implement this roll-up function.
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

-- Exercise 5: Abstract the commonality of these two validations into a higher-order function.
validate :: (ReceiveOrder -> Bool) -> [String] -> M.Map String [String] -> ReceiveOrder -> Either ReceiveOrderErrors ReceiveOrder
validate validator fullMessages errors receiveOrder
  | not $ validator receiveOrder =
    Left $ ReceiveOrderErrors {
      full_messages = fullMessages,
      errors = errors
    }
  | otherwise = Right receiveOrder

-- Exercise 4: Compose the two validations together.
validateReceiveOrder :: ReceiveOrder -> Either ReceiveOrderErrors ReceiveOrder
validateReceiveOrder = validateNumberOfItems >=> validatePositiveUnitQuantities

-- Exercise 2: Implement this validation.
validateNumberOfItems :: ReceiveOrder -> Either ReceiveOrderErrors ReceiveOrder
validateNumberOfItems = validate ((< maxNumberOfReceiveOrderItems) . length . receiveOrderItems)
  [ "Can only have 100 order items per Receive Order" ]
  (M.singleton "base" [ "Can only have 100 order items per Receive Order" ])

-- Exercise 3: Implement this validation too.
validatePositiveUnitQuantities :: ReceiveOrder -> Either ReceiveOrderErrors ReceiveOrder
validatePositiveUnitQuantities = validate (all ((> 0) . value . quantity) . receiveOrderItems)
  [ "Receive Order Item unit quantity must be greater than or equal to 0" ]
  (M.singleton "receive_order_item.unit_quantity" [ "unit quantity must be greater than or equal to 0" ])

