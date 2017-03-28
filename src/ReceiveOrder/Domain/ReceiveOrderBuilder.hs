module ReceiveOrder.Domain.ReceiveOrderBuilder(buildReceiveOrder) where

import ReceiveOrder.Domain.Types

import Control.Monad

import Data.Function
import Data.List
import qualified Data.List.NonEmpty as N
import Data.Ord
import qualified Data.Map.Strict as M

maxNumberOfReceiveOrderItems :: Int
maxNumberOfReceiveOrderItems = 100

-- Exercise 1: Wire it all together.
buildReceiveOrder :: ReceiveOrderAttributes -> Either ReceiveOrderErrors ReceiveOrder
buildReceiveOrder = error "Implement exercise 1" . receiveOrderFromAttributes

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

-- Exercise 2: Implement this validation.
validateNumberOfItems :: ReceiveOrder -> Either ReceiveOrderErrors ReceiveOrder
validateNumberOfItems ro@ReceiveOrder { receiveOrderItems = items }
    | error "Implement exercise 2" =
      Left ReceiveOrderErrors {
        full_messages = [ "Can only have 100 order items per Receive Order" ],
        errors = M.singleton "base" [ "Can only have 100 order items per Receive Order" ]
      }
    | otherwise = Right ro



-- Exercise 3: Implement this validation too.
validatePositiveUnitQuantities :: ReceiveOrder -> Either ReceiveOrderErrors ReceiveOrder
validatePositiveUnitQuantities ro@ReceiveOrder { receiveOrderItems = items }
  | error "Implement exercise 3" =
    Left ReceiveOrderErrors {
      full_messages = [ "Receive Order Item unit quantity must be greater than or equal to 0" ],
      errors = M.singleton "receive_order_item.unit_quantity" [ "unit quantity must be greater than or equal to 0" ]
    }
  | otherwise = Right ro

validateReceiveOrder :: ReceiveOrder -> Either ReceiveOrderErrors ReceiveOrder
validateReceiveOrder = validateNumberOfItems >=> validatePositiveUnitQuantities

-- Exercise 4: Abstract the commonality of these two validations into a higher-order function.
type Messages = [String]
type ErrorsMap = M.Map String Messages
validate :: (ReceiveOrder -> Bool) -> Messages -> ErrorsMap -> ReceiveOrder -> Either ReceiveOrderErrors ReceiveOrder
validate = error "Implement exercise 4"

rollUpQuantities :: ReceiveOrder -> ReceiveOrder
rollUpQuantities ro@ReceiveOrder { receiveOrderItems = items }
  | null items = ro
  | otherwise = ro { receiveOrderItems = (fmap rollUp . fmap N.fromList . groupBySku . sortBySku) items }
  where

  sortBySku :: [ReceiveOrderItem] -> [ReceiveOrderItem]
  sortBySku = sortBy (comparing skuId)

  groupBySku :: [ReceiveOrderItem] -> [[ReceiveOrderItem]]
  groupBySku = groupBy ((==) `on` skuId)

  rollUp :: N.NonEmpty ReceiveOrderItem -> ReceiveOrderItem
  rollUp (item N.:| items) = item {
    quantity = Quantity {
      unitOfMeasure = unitOfMeasure . quantity $ item,
      value         = orderQuantity item + sum (fmap orderQuantity items)
    }
  }

  orderQuantity :: ReceiveOrderItem -> Double
  orderQuantity = value . quantity
