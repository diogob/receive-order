{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module ApiSpec (spec) where

import Hasql.Pool (acquire)

import ReceiveOrder.Api
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

spec :: Spec
spec = with (app <$> acquire (10, 10, "postgres://localhost/receive_order_test")) $
    describe "POST /receive_orders" $ do
        it "responds with 200" $
            post "/receive_orders" [json|""|] `shouldRespondWith` 200
        it "responds with [ReceiveOrder]" $ do
            let users = [json| [{"vendor":"test vendor","expectedDeliveryAt":null,"reference":null,"receiveOrderItems":[{"sku":"testsku","quantity":{"value":1.0,"unitOfMeasure":"uomkey"}}]}] |]
            post "/receive_orders" [json|""|] `shouldRespondWith` users

  where
    app = serve api . server
