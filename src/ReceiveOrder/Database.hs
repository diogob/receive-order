{-# LANGUAGE TemplateHaskell #-}

module ReceiveOrder.Database where

import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Text
import Hasql.Pool

data Error = Error Text

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

createUser :: Pool -> [User] -> Either Error [User]
createUser _ _ = Right users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
