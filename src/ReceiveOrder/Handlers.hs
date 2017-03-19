module ReceiveOrder.Handlers
  ( massCreate
  , User
  ) where

import Servant

import ReceiveOrder.Database

massCreate :: ([User] -> Either Error [User]) -> Handler [User]
massCreate _ = return users

{-
we should just mapLeft on the input function
massCreate = do
  _ <- throwError $ err503 { errBody = errorMessage }
  where
    errorMessage = encode $ object ["message" .= ("Sorry" :: Text)]
-}
