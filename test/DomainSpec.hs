module DomainSpec (spec) where

import ReceiveOrder.Domain

import qualified Data.Map as M

import Test.Hspec

spec :: Spec
spec = describe "building a Receive Order from attributes" $ do
  describe "when passed valid attributes" $ do
    it "can create a Receive Order from the attributes provided" $ do
      let attributes = ReceiveOrderAttributes {
        vendor_name         = "Main Vendor",
        receive_order_items = [
          ReceiveOrderItemAttributes {
            sku_id                          = 1,
            unit_quantity_value             = 23.0,
            unit_of_measure_integration_key = "Default UoM"
          },
          ReceiveOrderItemAttributes {
            sku_id                          = 2,
            unit_quantity_value             = 42.0,
            unit_of_measure_integration_key = "Default UoM"
          }
        ]
      }

      buildReceiveOrder attributes `shouldBe` (Right ReceiveOrder {
        vendor             = "Main Vendor",
        expectedDeliveryAt = Nothing,
        reference          = Nothing,
        receiveOrderItems  = [
          ReceiveOrderItem {
            skuId    = 1,
            quantity = Quantity { value = 23.0, unitOfMeasure = "Default UoM" }
          },
          ReceiveOrderItem {
            skuId    = 2,
            quantity = Quantity { value = 42.0, unitOfMeasure = "Default UoM" }
          }
        ]
      })

    it "rolls up Receive Order Item quantities for the same Sku" $ do
      let attributes = ReceiveOrderAttributes {
        vendor_name         = "Main Vendor",
        receive_order_items = [
          ReceiveOrderItemAttributes {
            sku_id                          = 1,
            unit_quantity_value             = 23.0,
            unit_of_measure_integration_key = "Default UoM"
          },
          ReceiveOrderItemAttributes {
            sku_id                          = 1,
            unit_quantity_value             = 42.0,
            unit_of_measure_integration_key = "Default UoM"
          }
        ]
      }

      buildReceiveOrder attributes `shouldBe` (Right ReceiveOrder {
        vendor             = "Main Vendor",
        expectedDeliveryAt = Nothing,
        reference          = Nothing,
        receiveOrderItems  = [
          ReceiveOrderItem {
            skuId    = 1,
            quantity = Quantity { value = 65.0, unitOfMeasure = "Default UoM" }
          }
        ]
      })
      

  describe "when passed invalid attributes" $ do
    it "rejects Receive Orders that have over 100 Receive Order Items" $ do
      let createItemAttributes = (\n -> ReceiveOrderItemAttributes {
          sku_id                          = n,
          unit_quantity_value             = 23.0,
          unit_of_measure_integration_key = "Default UoM"
      })
      let attributes = ReceiveOrderAttributes {
        vendor_name         = "Main Vendor",
        receive_order_items = map createItemAttributes [0..101]
      }

      buildReceiveOrder attributes `shouldBe` (Left ReceiveOrderErrors {
        full_messages = [ "Can only have 100 order items per Receive Order" ],
        errors = M.singleton "base" [ "Can only have 100 order items per Receive Order" ]
      })
