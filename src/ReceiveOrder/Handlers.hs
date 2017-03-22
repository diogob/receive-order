module ReceiveOrder.Handlers
  ( massCreate
  ) where

import Servant

import ReceiveOrder.Database
import Domain

massCreate :: ([ReceiveOrderAttributes] -> Either Error [ReceiveOrder]) -> Handler [ReceiveOrder]
massCreate _ = return receiveOrders

{-
we should just mapLeft on the input function
massCreate = do
  _ <- throwError $ err503 { errBody = errorMessage }
  where
    errorMessage = encode $ object ["message" .= ("Sorry" :: Text)]
-}
