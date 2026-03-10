{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module DataTypes
  ( TicketParams(..)
  , EventParams(..)
  , BuyParams(..)
  , TicketSale(..)
  , SaleAction(..)
  , StartSaleParams(..)
  , BuyTicketByNameParams(..)
  , TicketNumber
  , EventName
  , TicketData(..)
  ) where

import           Data.Aeson                 (ToJSON, FromJSON)
import           GHC.Generics               (Generic)
import           Schema                     (ToSchema)
import           Ledger                     (PaymentPubKeyHash, TokenName, CurrencySymbol)
import           Plutus.V1.Ledger.Time      (POSIXTime)
import           PlutusTx.Prelude           hiding (Show)
import qualified PlutusTx
import           Prelude                    (Show, Eq, Ord, String)

-------------------------------------------------
-- Type Aliases
-------------------------------------------------

type EventName = String
type TicketNumber = Integer

-------------------------------------------------
-- Ticket Data
-------------------------------------------------

data TicketData = TicketData
  { tCurrSym :: !CurrencySymbol
  , tToken   :: !TokenName
  }
  deriving (Show, Generic, Eq)

-------------------------------------------------
-- Ticket Minting Parameters
-------------------------------------------------

data TicketParams = TicketParams
  { tpTicketName :: !TokenName
  , tpTicketNum  :: !Integer
  , tpDeadline   :: !POSIXTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Eq)

-------------------------------------------------
-- Event Parameters
-------------------------------------------------

data EventParams = EventParams
  { epEventName       :: !EventName
  , epEventTime       :: !POSIXTime
  , epNumberOfTickets :: !Integer
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Eq)

-------------------------------------------------
-- Sale Initialization
-------------------------------------------------

data StartSaleParams = StartSaleParams
  { saPrice :: !Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema, Eq, Ord)

-------------------------------------------------
-- Buy Ticket (using ticket name)
-------------------------------------------------

data BuyTicketByNameParams = BuyTicketByNameParams
  { btTicketName    :: !String
  , btSellerAddress :: !PaymentPubKeyHash
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema, Eq, Ord)

-------------------------------------------------
-- Buy Ticket (using currency + token)
-------------------------------------------------

data BuyParams = BuyParams
  { bCurrSym       :: !CurrencySymbol
  , bToken         :: !TokenName
  , bSellerAddress :: !PaymentPubKeyHash
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema, Eq, Ord)

-------------------------------------------------
-- Ticket Sale Datum
-------------------------------------------------

data TicketSale = TicketSale
  { nSeller   :: !PaymentPubKeyHash
  , nPrice    :: !Integer
  , nCurrency :: !CurrencySymbol
  , nToken    :: !TokenName
  }
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

-------------------------------------------------
-- Redeemer
-------------------------------------------------

data SaleAction = Buy
  deriving Show

-------------------------------------------------
-- Plutus Template Haskell
-------------------------------------------------

PlutusTx.makeIsDataIndexed ''TicketSale [('TicketSale, 0)]
PlutusTx.makeLift ''TicketSale

PlutusTx.makeIsDataIndexed ''SaleAction [('Buy, 0)]
PlutusTx.makeLift ''SaleAction
