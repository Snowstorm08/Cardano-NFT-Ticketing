{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Trace
    ( 
        testCreateEvent,
        testCreateEventAfterDeadline,
        testValidBuy,
        testStartSale,
        testInvalidBuy,
        testStartSaleAllTickets
    ) where

import           Offchain
import           Control.Monad          (void)
import           Control.Monad.Freer.Extras as Extras
import           Wallet.Emulator.Wallet
import           Plutus.Trace.Emulator  as Emulator
import           PlutusTx.Prelude       as Plutus (($), Either(..) )
import           Ledger                 hiding (singleton)
import           Prelude                (IO, String, show)
import           Data.Default           (def)
import qualified Data.Map               as Map
import           Ledger.Ada             as Ada
import           Text.Printf            (printf)
import           DataTypes

testEventName :: String
testEventName = "Event2023"

testTokenName :: TokenName
testTokenName = "Event2023_1"

testCurrencySymbol :: CurrencySymbol
testCurrencySymbol = "a8038825de6ee33c0f4970c8ddec533366e0b7a436a1df8b59a6808e"

testEventTimeFuture :: POSIXTime
testEventTimeFuture = 1885547004000

testEventTimeInPast :: POSIXTime
testEventTimeInPast = 1576059097899

-- When testing, the simulated POSIXTime starts at 1596059101 (July 29th 2020 the beginning of the Shelley era).
-- Keep this in mind when testing POSIX times.

testCreateEvent :: IO ()
testCreateEvent = do
    let dist = Map.fromList [ (knownWallet 1, Ada.lovelaceValueOf 10000000)]
        emCfg = EmulatorConfig (Left dist) def def
    runEmulatorTraceIO' def emCfg $ do
        h1 <- activateContractWallet (knownWallet 1) endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"createEvent" h1 EventParams{ 
          epEventName           = testEventName,
          epEventTime           = testEventTimeFuture,
          epNumberOfTickets     = 5
        }
        void $ Emulator.waitNSlots 10
        let wallet1 = knownWallet 1
        Extras.logInfo @String $ printf "Wallet 1 (%s) created 5 tickets." (show wallet1)

testCreateEventAfterDeadline :: IO ()
testCreateEventAfterDeadline = do
    let dist = Map.fromList [ (knownWallet 1, Ada.lovelaceValueOf 10000000)]
        emCfg = EmulatorConfig (Left dist) def def
    runEmulatorTraceIO' def emCfg $ do
        h1 <- activateContractWallet (knownWallet 1) endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"createEvent" h1 EventParams{ 
          epEventName           = testEventName,
          epEventTime           = testEventTimeInPast,
          epNumberOfTickets     = 5
        }
        void $ Emulator.waitNSlots 10
        let wallet1 = knownWallet 1
        Extras.logInfo @String $ printf "Wallet 1: %s" (show wallet1)

testStartSale :: IO ()
testStartSale = do
    let dist = Map.fromList [(knownWallet 1, Ada.lovelaceValueOf 100000000), (knownWallet 2, Ada.lovelaceValueOf 10000000)]
        emCfg = EmulatorConfig (Left dist) def def
    runEmulatorTraceIO' def emCfg $ do
        h2 <- activateContractWallet (knownWallet 2) endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"createEvent" h2 EventParams{ 
          epEventName           = testEventName,
          epEventTime           = testEventTimeFuture,
          epNumberOfTickets     = 1
        }
        void $ Emulator.waitNSlots 2
        callEndpoint @"startSale" h2 StartParams{
           sPrice         = 5000000, -- 5 ADA
           sCurrSym       = "6c15bac02cd5b903bfcac6727d4c44c201ba3c2cf10479882214bfbb",
           sToken         =  testTokenName
        }
        void $ Emulator.waitNSlots 2
        let wallet1 = knownWallet 1
        Extras.logInfo @String $ printf "Wallet 2 (%s) started the sale of 1 ticket." (show wallet1)

testStartSaleAllTickets :: IO ()
testStartSaleAllTickets = do
    let dist = Map.fromList [(knownWallet 1, Ada.lovelaceValueOf 100000000), (knownWallet 2, Ada.lovelaceValueOf 10000000)]
        emCfg = EmulatorConfig (Left dist) def def
    runEmulatorTraceIO' def emCfg $ do
        h2 <- activateContractWallet (knownWallet 2) endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"createEvent" h2 EventParams{ 
          epEventName           = testEventName,
          epEventTime           = testEventTimeFuture,
          epNumberOfTickets     = 5
        }
        void $ Emulator.waitNSlots 6
        callEndpoint @"startSale" h2 StartParams{
           sPrice         = 5000000, -- 5 ADA
           sCurrSym       = "test",
           sToken         = testTokenName
        }
        void $ Emulator.waitNSlots 2

testValidBuy :: IO ()
testValidBuy = do
    let dist = Map.fromList [(knownWallet 1, Ada.lovelaceValueOf 100000000), (knownWallet 2, Ada.lovelaceValueOf 10000000)]
        emCfg = EmulatorConfig (Left dist) def def
    runEmulatorTraceIO' def emCfg $ do
        h1 <- activateContractWallet (knownWallet 1) endpoints
        h2 <- activateContractWallet (knownWallet 2) endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"createEvent" h1 EventParams{ 
          epEventName           = testEventName,
          epEventTime           = testEventTimeFuture,
          epNumberOfTickets     = 1
        }
        void $ Emulator.waitNSlots 2
        callEndpoint @"startSale" h1 StartParams{
           sPrice         = 5000000, -- 5 ADA
           sCurrSym       = testCurrencySymbol,
           sToken         = testTokenName
        }
        void $ Emulator.waitNSlots 2
        callEndpoint @"buyTicket" h2 BuyParams{
           bToken    = testTokenName,
           bCurrSym  = testCurrencySymbol,
           bSellerAddress = mockWalletPaymentPubKeyHash $ knownWallet 1
        }
        void $ Emulator.waitNSlots 2
        let wallet1 = knownWallet 1
        let wallet2 = knownWallet 2
        Extras.logInfo @String $ printf "Wallet 2 (%s) bought ticket from wallet 1 (%s)" (show wallet2) (show wallet1)

testInvalidBuy :: IO ()
testInvalidBuy = do
    -- Wallet 3 tries to buy a ticket that is already sold
    let dist = Map.fromList [(knownWallet 1, Ada.lovelaceValueOf 100000000), (knownWallet 2, Ada.lovelaceValueOf 10000000), (knownWallet 3, Ada.lovelaceValueOf 10000000)]
        emCfg = EmulatorConfig (Left dist) def def
    runEmulatorTraceIO' def emCfg $ do
        h1 <- activateContractWallet (knownWallet 1) endpoints
        h2 <- activateContractWallet (knownWallet 2) endpoints
        h3 <- activateContractWallet (knownWallet 3) endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"createEvent" h1 EventParams{ 
          epEventName           = testEventName,
          epEventTime           = testEventTimeFuture,
          epNumberOfTickets     = 1
        }
        void $ Emulator.waitNSlots 2
        callEndpoint @"startSale" h1 StartParams{
           sPrice         = 5000000, -- 5 ADA
           sCurrSym       = testCurrencySymbol,
           sToken         = testTokenName
        }
        void $ Emulator.waitNSlots 2
        callEndpoint @"buyTicket" h2 BuyParams{
           bToken    = testTokenName,
           bCurrSym  = testCurrencySymbol,
           bSellerAddress = mockWalletPaymentPubKeyHash $ knownWallet 1
        }
        void $ Emulator.waitNSlots 2
        -- Wallet 3 tries to buy the same ticket
        callEndpoint @"buyTicket" h3 BuyParams{
           bToken    = testTokenName,
           bCurrSym  = testCurrencySymbol,
           bSellerAddress = mockWalletPaymentPubKeyHash $ knownWallet 1
        }
        void $ Emulator.waitNSlots 2

        let wallet1 = knownWallet 1
        let wallet2 = knownWallet 2
        let wallet3 = knownWallet 3
        Extras.logInfo @String $ printf "Wallet 2 (%s) bought ticket from wallet 1 (%s). Wallet 3 (%s) tries to buy the same ticket." (show wallet2) (show wallet1) (show wallet3)