module DomainSpec (spec) where

import Domain

import qualified Data.Map as M

import Test.Hspec

spec :: Spec
spec = describe "building a Receive Order from attributes" $ do
  describe "when passed valid attributes" $ do
    it "can create a Receive Order from the attributes provided" $ do
      let attributes = ReceiveOrderAttributes {
        vendorName                  = "Main Vendor",
        receiveOrderItemsAttributes = [
          ReceiveOrderItemAttributes {
            skuCode                     = "First Sku",
            unitQuantityValue           = 23.0,
            unitOfMeasureIntegrationKey = "Default UoM"
          },
          ReceiveOrderItemAttributes {
            skuCode                     = "Second Sku",
            unitQuantityValue           = 42.0,
            unitOfMeasureIntegrationKey = "Default UoM"
          }
        ]
      }

      buildReceiveOrder attributes `shouldBe` (Right ReceiveOrder {
        vendor             = "Main Vendor",
        expectedDeliveryAt = Nothing,
        reference          = Nothing,
        receiveOrderItems  = [
          ReceiveOrderItem {
            sku      = "First Sku",
            quantity = Quantity { value = 23.0, unitOfMeasure = "Default UoM" }
          },
          ReceiveOrderItem {
            sku      = "Second Sku",
            quantity = Quantity { value = 42.0, unitOfMeasure = "Default UoM" }
          }
        ]
      })

  describe "when passed invalid attributes" $ do
    it "rejects Receive Orders that have over 100 Receive Order Items" $ do
      let attributes = ReceiveOrderAttributes {
        vendorName                  = "Main Vendor",
        receiveOrderItemsAttributes = take 101 . repeat $ ReceiveOrderItemAttributes {
          skuCode                     = "First Sku",
          unitQuantityValue           = 23.0,
          unitOfMeasureIntegrationKey = "Default UoM"
        }
      }

      buildReceiveOrder attributes `shouldBe` (Left ReceiveOrderErrors {
        fullMessages = [ "can only have 100 order items per receive order" ],
        errors = M.singleton "Receive Order" [ "can only have 100 order items per receive order" ]
      })
