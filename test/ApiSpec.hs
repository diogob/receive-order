{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module ApiSpec (spec) where

import Hasql.Pool (acquire)

import Domain
import qualified Data.Map.Strict as M
import ReceiveOrder.Api
import ReceiveOrder.Handlers
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Data.Aeson (encode)
import Network.HTTP.Types

spec :: Spec
spec = with (app <$> acquire (10, 10, "postgres://localhost/receive_order_test")) $
    describe "POST /receive_orders" $ do
        it "responds with 200" $
            postReceiveOrders receiveOrderAttributes `shouldRespondWith` 200
        it "responds with an object mapping user-provided CIDs to ReceiveOrders" $ do
            let receiveOrders = [json|
              { "cid_1": {
                "vendor":"test vendor",
                "expectedDeliveryAt":null,
                "reference":null,
                "receiveOrderItems": [{
                  "sku":"testsku",
                  "quantity": {
                    "value": 1.0,
                    "unitOfMeasure": "uomkey"
                  }
                }]
              }}|]

            postReceiveOrders receiveOrderAttributes `shouldRespondWith` receiveOrders

        it "responds with errors keyed by user-provided CIDs for invalid attributes" $ do
            let errors = [json|
              { "cid_1": {
                  "full_messages": [ "Can only have 100 order items per Receive Order" ],
                  "errors": {
                    "base": [ "Can only have 100 order items per Receive Order" ]
                  }
                }
              }|]

            postReceiveOrders erroneousAttributes `shouldRespondWith` errors
  where

    app = serve api . server
    postReceiveOrders = request methodPost "/receive_orders" [(hContentType, "application/json")] . encode

    receiveOrderAttributes :: ReceiveOrdersRequest
    receiveOrderAttributes = ReceiveOrdersRequest . M.singleton "cid_1" $
      ReceiveOrderAttributes {
        vendor_name = "test vendor",
        receive_order_items = [
          ReceiveOrderItemAttributes {
            skuCode = "testsku",
            unitQuantityValue = 1.0,
            unitOfMeasureIntegrationKey = "uomkey"
          }
        ]
      }

    erroneousAttributes :: ReceiveOrdersRequest
    erroneousAttributes = ReceiveOrdersRequest . M.singleton "cid_1" $ 
      ReceiveOrderAttributes {
        vendor_name = "test vendor",
        receive_order_items = (map (const ReceiveOrderItemAttributes 
          {
            skuCode = "testsku",
            unitQuantityValue = 1.0,
            unitOfMeasureIntegrationKey = "uomkey"
          }) [0..100])
      }
