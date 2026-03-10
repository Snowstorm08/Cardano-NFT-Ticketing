{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Trace
    ( testCreateEvent
    , testCreateEventAfterDeadline
    , testValidBuy
    , testStartSale
    , testInvalidBuy
    ) where

import           Offchain
import           Control.Monad              (void)
import qualified Control.Monad.Freer.Extras as Extras
import           Wallet.Emulator.Wallet
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           as Plutus (($), Either(..))
import           Ledger                     hiding (singleton)

import           Prelude                    (IO, String, show)
import           Data.Default               (def)
import qualified Data.Map                   as Map
import           Ledger.Ada                 as Ada
import           Text.Printf                (printf)

import           DataTypes

--------------------------------------------------------------------------------
-- Test Constants
--------------------------------------------------------------------------------

testEventName :: String
testEventName = "Event2023"

testEventTimeFuture :: POSIXTime
testEventTimeFuture = 1885547004000

testEventTimePast :: POSIXTime
testEventTimePast = 1576059097899

ticketPrice :: Integer
ticketPrice = 5_000_000 -- 5 ADA


--------------------------------------------------------------------------------
-- Emulator Setup Helper
--------------------------------------------------------------------------------

runTrace :: Map.Map Wallet Value -> EmulatorTrace () -> IO ()
runTrace dist trace =
    runEmulatorTraceIO' def (EmulatorConfig (Left dist) def def) trace


--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testCreateEvent :: IO ()
testCreateEvent =
    runTrace dist $ do
        h1 <- activateContractWallet w1 endpoints

        void $ Emulator.waitNSlots 1

        callEndpoint @"createEvent" h1 EventParams
            { epEventName       = testEventName
            , epEventTime       = testEventTimeFuture
            , epNumberOfTickets = 5
            }

        void $ Emulator.waitNSlots 10

        Extras.logInfo @String $
            printf "Wallet 1 (%s) created 5 tickets." (show w1)

  where
    w1   = knownWallet 1
    dist = Map.fromList [(w1, Ada.lovelaceValueOf 10_000_000)]


--------------------------------------------------------------------------------

testCreateEventAfterDeadline :: IO ()
testCreateEventAfterDeadline =
    runTrace dist $ do
        h1 <- activateContractWallet w1 endpoints

        void $ Emulator.waitNSlots 1

        callEndpoint @"createEvent" h1 EventParams
            { epEventName       = testEventName
            , epEventTime       = testEventTimePast
            , epNumberOfTickets = 5
            }

        void $ Emulator.waitNSlots 10

        Extras.logInfo @String $
            printf "Wallet 1: %s" (show w1)

  where
    w1   = knownWallet 1
    dist = Map.fromList [(w1, Ada.lovelaceValueOf 10_000_000)]


--------------------------------------------------------------------------------

testStartSale :: IO ()
testStartSale =
    runTrace dist $ do
        h2 <- activateContractWallet w2 endpoints

        void $ Emulator.waitNSlots 1

        callEndpoint @"createEvent" h2 EventParams
            { epEventName       = testEventName
            , epEventTime       = testEventTimeFuture
            , epNumberOfTickets = 5
            }

        void $ Emulator.waitNSlots 6

        callEndpoint @"startSaleAllTickets" h2 StartSaleParams
            { saPrice = ticketPrice
            }

        void $ Emulator.waitNSlots 6

        Extras.logInfo @String $
            printf "Wallet 2 (%s) started ticket sale." (show w2)

  where
    w1   = knownWallet 1
    w2   = knownWallet 2
    dist = Map.fromList
        [ (w1, Ada.lovelaceValueOf 100_000_000)
        , (w2, Ada.lovelaceValueOf 90_000_000)
        ]


--------------------------------------------------------------------------------

testValidBuy :: IO ()
testValidBuy =
    runTrace dist $ do
        h1 <- activateContractWallet w1 endpoints
        h2 <- activateContractWallet w2 endpoints

        void $ Emulator.waitNSlots 1

        callEndpoint @"createEvent" h1 EventParams
            { epEventName       = testEventName
            , epEventTime       = testEventTimeFuture
            , epNumberOfTickets = 5
            }

        void $ Emulator.waitNSlots 6

        callEndpoint @"startSaleAllTickets" h1 StartSaleParams
            { saPrice = ticketPrice
            }

        void $ Emulator.waitNSlots 6

        callEndpoint @"buyTicketByName" h2 BuyTicketByNameParams
            { btTicketName    = "Event2023_2"
            , btSellerAddress = mockWalletPaymentPubKeyHash w1
            }

        void $ Emulator.waitNSlots 4

        Extras.logInfo @String $
            printf "Wallet 2 (%s) bought ticket from wallet 1 (%s)"
                (show w2) (show w1)

  where
    w1   = knownWallet 1
    w2   = knownWallet 2
    dist = Map.fromList
        [ (w1, Ada.lovelaceValueOf 100_000_000)
        , (w2, Ada.lovelaceValueOf 90_000_000)
        ]


--------------------------------------------------------------------------------

testInvalidBuy :: IO ()
testInvalidBuy =
    runTrace dist $ do
        h1 <- activateContractWallet w1 endpoints
        h2 <- activateContractWallet w2 endpoints
        h3 <- activateContractWallet w3 endpoints

        void $ Emulator.waitNSlots 1

        callEndpoint @"createEvent" h1 EventParams
            { epEventName       = testEventName
            , epEventTime       = testEventTimeFuture
            , epNumberOfTickets = 5
            }

        void $ Emulator.waitNSlots 10

        callEndpoint @"startSaleAllTickets" h1 StartSaleParams
            { saPrice = ticketPrice
            }

        void $ Emulator.waitNSlots 6

        callEndpoint @"buyTicketByName" h2 BuyTicketByNameParams
            { btTicketName    = "Event2023_2"
            , btSellerAddress = mockWalletPaymentPubKeyHash w1
            }

        void $ Emulator.waitNSlots 4

        callEndpoint @"buyTicketByName" h3 BuyTicketByNameParams
            { btTicketName    = "Event2023_2"
            , btSellerAddress = mockWalletPaymentPubKeyHash w1
            }

        void $ Emulator.waitNSlots 4

        Extras.logInfo @String $
            printf
                "Wallet 2 (%s) bought ticket from wallet 1 (%s). Wallet 3 (%s) tried to buy the same ticket."
                (show w2) (show w1) (show w3)

  where
    w1   = knownWallet 1
    w2   = knownWallet 2
    w3   = knownWallet 3

    dist = Map.fromList
        [ (w1, Ada.lovelaceValueOf 100_000_000)
        , (w2, Ada.lovelaceValueOf 90_000_000)
        , (w3, Ada.lovelaceValueOf 90_000_000)
        ]
