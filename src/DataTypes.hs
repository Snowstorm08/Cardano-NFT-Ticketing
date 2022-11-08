{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module DataTypes
    ( TicketParams (..),
      EventParams (..),
      BuyParams (..),
      TicketSale (..),
      SaleAction (..),
      StartSaleParams (..),
      BuyTicketByNameParams (..),
      TicketNumber,
      EventName,
      TicketData (..)
    )
    where

import           Data.Aeson                (ToJSON, FromJSON)
import           GHC.Generics              (Generic)
import           Schema                    (ToSchema)
import           Ledger                    (PaymentPubKeyHash, TokenName, CurrencySymbol, AssetClass)
import           Prelude                   (Show (..), String, Eq, Ord)
import qualified PlutusTx
import           PlutusTx.Prelude          as Plutus (Integer)
import           Plutus.V1.Ledger.Time

type EventName = String
type TicketNumber = Integer

data TicketData = TicketData
   { 
      tCurrSym :: CurrencySymbol,
      tToken   :: TokenName 
   }

data TicketParams = TicketParams
    { 
      tpTicketName   :: !TokenName,
      tpTicketNum    :: !Integer,
      tpDeadline     :: !POSIXTime
    } 
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data EventParams = EventParams
    { 
      epEventName           :: !String,
      epEventTime           :: !POSIXTime,
      epNumberOfTickets     :: !Integer
    } 
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data StartSaleParams = StartSaleParams
    { 
      saPrice      :: Integer
    } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

data BuyTicketByNameParams = BuyTicketByNameParams
    {
      btTicketName    :: !String,
      btSellerAddress :: !PaymentPubKeyHash
    } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

data BuyParams = BuyParams
    { 
      bCurrSym       :: !CurrencySymbol,
      bToken         :: !TokenName, 
      bSellerAddress :: !PaymentPubKeyHash
    } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

data TicketSale = TicketSale
    { 
      nSeller    :: !PaymentPubKeyHash,
      nPrice     :: !Integer,
      nCurrency  :: !CurrencySymbol,
      nToken     :: !TokenName
    } deriving (Generic, ToJSON, FromJSON)

data SaleAction = Buy 
    deriving Show

PlutusTx.makeIsDataIndexed ''TicketSale [('TicketSale, 0)]
PlutusTx.makeLift ''TicketSale

PlutusTx.makeIsDataIndexed ''SaleAction [('Buy, 0)]
PlutusTx.makeLift ''SaleAction