{-# LANGUAGE TemplateHaskell #-}

module ReceiveOrder.Domain.Types
  ( ByCid(..)
  , Quantity(..)
  , ReceiveOrder(..)
  , ReceiveOrderAttributes(..)
  , ReceiveOrderErrors(..)
  , ReceiveOrderItem(..)
  , ReceiveOrderItemAttributes(..)
  , ReceiveOrdersRequest(..)
  ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)
import qualified Data.Map.Strict as M
import Data.Time.Clock

data ReceiveOrdersRequest = ReceiveOrdersRequest { receive_orders :: ByCid ReceiveOrderAttributes }

data ReceiveOrderAttributes = ReceiveOrderAttributes
  { vendor_name         :: String
  , receive_order_items :: [ReceiveOrderItemAttributes]
  } deriving (Show, Eq)

data ReceiveOrderItemAttributes = ReceiveOrderItemAttributes
  { sku_id                          :: Integer
  , unit_quantity_value             :: Double
  , unit_of_measure_integration_key :: String
  } deriving (Show, Eq)

data ReceiveOrderErrors = ReceiveOrderErrors
  { full_messages :: [String]
  , errors :: M.Map String [String] 
  } deriving (Show, Eq)

type ByCid = M.Map String

data ReceiveOrder = ReceiveOrder
  { vendor             :: String
  , expectedDeliveryAt :: Maybe UTCTime
  , reference          :: Maybe String
  , receiveOrderItems  :: [ReceiveOrderItem]
  } deriving(Eq, Show)

data ReceiveOrderItem = ReceiveOrderItem
  { skuId    :: Integer
  , quantity :: Quantity
  } deriving(Eq, Show)

data Quantity = Quantity
  { value         :: Double
  , unitOfMeasure :: String
  } deriving(Eq, Show)

$(deriveJSON defaultOptions ''ReceiveOrderAttributes)
$(deriveJSON defaultOptions ''ReceiveOrderItemAttributes)
$(deriveJSON defaultOptions ''ReceiveOrderErrors)
$(deriveJSON defaultOptions ''ReceiveOrder)
$(deriveJSON defaultOptions ''ReceiveOrderItem)
$(deriveJSON defaultOptions ''ReceiveOrdersRequest)
$(deriveJSON defaultOptions ''Quantity)
