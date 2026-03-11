{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module DataTypes
  ( -- * Type Aliases
    EventName
  , TicketNumber
    -- * Core Data Types
  , TicketData(..)
  , TicketParams(..)
  , EventParams(..)
  , TicketSale(..)
  , SaleAction(..)
    -- * Endpoint Parameters
  , StartSaleParams(..)
  , BuyParams(..)
  , BuyTicketByNameParams(..)
  ) where

import           Data.Aeson            (FromJSON, ToJSON)
import           GHC.Generics          (Generic)
import           Ledger                (CurrencySymbol, PaymentPubKeyHash, TokenName)
import           Plutus.V1.Ledger.Time (POSIXTime)
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (Show)
import           Prelude               (Eq, Ord, Show, String)
import           Schema                (ToSchema)

-- | Human-readable event identifier.
type EventName    = String

-- | Numeric ticket identifier within an event.
type TicketNumber = Integer

---------------------------------------------------------------------------
-- On-chain / Reference Types
---------------------------------------------------------------------------

-- | Lightweight reference to a minted ticket (currency symbol + token name).
data TicketData = TicketData
  { tCurrSym :: !CurrencySymbol
  , tToken   :: !TokenName
  } deriving stock    (Show, Eq, Generic)

-- | On-chain datum attached to a ticket-sale UTXO.
data TicketSale = TicketSale
  { nSeller   :: !PaymentPubKeyHash
  , nPrice    :: !Integer
  , nCurrency :: !CurrencySymbol
  , nToken    :: !TokenName
  } deriving stock    (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | On-chain redeemer for the sale validator.
data SaleAction = Buy
  deriving stock (Show)

---------------------------------------------------------------------------
-- Off-chain / Endpoint Parameter Types
---------------------------------------------------------------------------

-- | Parameters for minting a batch of tickets.
data TicketParams = TicketParams
  { tpTicketName :: !TokenName
  , tpTicketNum  :: !Integer
  , tpDeadline   :: !POSIXTime
  } deriving stock    (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Parameters for creating a new event.
data EventParams = EventParams
  { epEventName       :: !EventName
  , epEventTime       :: !POSIXTime
  , epNumberOfTickets :: !Integer
  } deriving stock    (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Parameters for listing a ticket for sale.
data StartSaleParams = StartSaleParams
  { saPrice :: !Integer
  } deriving stock    (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Buy a ticket by currency symbol and token name.
data BuyParams = BuyParams
  { bCurrSym       :: !CurrencySymbol
  , bToken         :: !TokenName
  , bSellerAddress :: !PaymentPubKeyHash
  } deriving stock    (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Buy a ticket by its human-readable name.
data BuyTicketByNameParams = BuyTicketByNameParams
  { btTicketName    :: !String
  , btSellerAddress :: !PaymentPubKeyHash
  } deriving stock    (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

---------------------------------------------------------------------------
-- Plutus TH – Serialisation & Lifting
---------------------------------------------------------------------------

PlutusTx.makeIsDataIndexed ''TicketSale  [('TicketSale, 0)]
PlutusTx.makeLift          ''TicketSale

PlutusTx.makeIsDataIndexed ''SaleAction  [('Buy, 0)]
PlutusTx.makeLift          ''SaleAction
