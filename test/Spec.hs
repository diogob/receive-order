{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import ReceiveOrder (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $
    describe "POST /users" $ do
        it "responds with 200" $
            post "/users" [json|""|] `shouldRespondWith` 200
        it "responds with [User]" $ do
            let users = [json| [{"userId":1,"userFirstName":"Isaac","userLastName":"Newton"},{"userId":2,"userFirstName":"Albert","userLastName":"Einstein"}] |]
            post "/users" [json|""|] `shouldRespondWith` users
