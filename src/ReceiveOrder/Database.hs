module ReceiveOrder.Database where

import Domain
import Data.Text
import Hasql.Pool

newtype Error = Error Text

createUser :: Pool -> [ReceiveOrderAttributes] -> Either Error [ReceiveOrder]
createUser _ _ = Right receiveOrders

receiveOrders :: [ReceiveOrder]
receiveOrders = [
  buildReceiveOrder ReceiveOrderAttributes { vendorName = "test vendor"
                                           , receiveOrderItemsAttributes = [
                                               ReceiveOrderItemAttributes { skuCode = "testsku"
                                                                          , unitQuantityValue = 1.0
                                                                          , unitOfMeasureIntegrationKey = "uomkey"
                                                                          }
                                               ]
                                           }
  ]
