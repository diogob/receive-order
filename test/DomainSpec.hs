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
        receiveOrderItemsAttributes = map (\n -> ReceiveOrderItemAttributes {
          skuCode                     = "Sku " ++ (show n),
          unitQuantityValue           = 23.0,
          unitOfMeasureIntegrationKey = "Default UoM"
        }) [0..101]
      }

      buildReceiveOrder attributes `shouldBe` (Left ReceiveOrderErrors {
        full_messages = [ "Can only have 100 order items per Receive Order" ],
        errors = M.singleton "base" [ "Can only have 100 order items per Receive Order" ]
      })

    it "requires that all of the Receive Order's Items' Skus are unique" $ do
      let attributes = ReceiveOrderAttributes {
        vendorName                  = "Main Vendor",
        receiveOrderItemsAttributes = [
          ReceiveOrderItemAttributes {
            skuCode                     = "First Sku",
            unitQuantityValue           = 23.0,
            unitOfMeasureIntegrationKey = "Default UoM"
          },
          ReceiveOrderItemAttributes {
            skuCode                     = "First Sku",
            unitQuantityValue           = 42.0,
            unitOfMeasureIntegrationKey = "Eaches UoM"
          }
        ]
      }

      buildReceiveOrder attributes `shouldBe` (Left ReceiveOrderErrors {
        full_messages = [ "Sku already exists for this Receive Order" ],
        errors = M.singleton "sku" [ "already exists for this Receive Order" ]
      })
