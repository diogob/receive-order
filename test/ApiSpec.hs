{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module ApiSpec (spec) where

import Hasql.Pool (acquire)

import ReceiveOrder.Api
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

spec :: Spec
spec = with ((acquire (10, 10, "postgres://localhost/receive_order_test")) >>= (return . app)) $
    describe "POST /users" $ do
        it "responds with 200" $
            post "/users" [json|""|] `shouldRespondWith` 200
        it "responds with [User]" $ do
            let users = [json| [{"userId":1,"userFirstName":"Isaac","userLastName":"Newton"},{"userId":2,"userFirstName":"Albert","userLastName":"Einstein"}] |]
            post "/users" [json|""|] `shouldRespondWith` users

  where
    app = serve api . server
