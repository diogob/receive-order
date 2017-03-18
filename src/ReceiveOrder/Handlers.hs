{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings   #-}
module ReceiveOrder.Handlers
  ( massCreate
  , User
  ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Text

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

massCreate :: Handler [User]
massCreate = return users

{-
massCreate = do
  _ <- throwError $ err503 { errBody = errorMessage }
  where
    errorMessage = encode $ object ["message" .= ("Sorry" :: Text)]
-}

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
