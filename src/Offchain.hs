{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Offchain
    ( 
        endpoints
    )
    where

import           Control.Monad          hiding (fmap, sequence_)
import           Data.Aeson             (ToJSON)
import           Data.Text              (Text)
import           Data.String            (IsString (..))
import qualified Data.Map               as Map
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Plutus.ChainIndex.Tx   (ChainIndexTx(_citxData) )
import           Ledger                 hiding (mint, singleton)
import qualified Ledger.Constraints     as Constraints
import           Ledger.Value           as Value
import           Ledger.Ada             as Ada
import           Prelude                (Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           DataTypes
import           Onchain

type ProductSchema =
        Endpoint "createEvent" EventParams
    .\/ Endpoint "buyTicket"   BuyParams
    .\/ Endpoint "startSale"   StartParams

-- Create the tickets (NFT's) based on the provided event parameters
createEvent :: EventParams -> Contract w ProductSchema Text ()
createEvent ep = do    
    let iTicketCount = epNumberOfTickets ep
    let sName = epEventName ep
    let pEventTime = epEventTime ep
    logInfo @String $ printf "Minting tickets for event: %s. Number of tickets: %s" (show sName) (show iTicketCount)
    createTickets sName [0 .. iTicketCount - 1] pEventTime

-- Create the event tickets (recursive)
-- Inputs : Event Name, Number of Tickets, Deadline for minting
createTickets :: String -> [Integer] -> POSIXTime -> Contract w ProductSchema Text ()
createTickets eventName xs eventTime = do
    let iTicketNumber = head xs + 1
    let sTicketName = eventName ++ "_" ++ show iTicketNumber
    let tTicket = TicketParams (fromString sTicketName) iTicketNumber eventTime
    mintTicket tTicket
    if (tail xs) == []
        then logInfo @String $ "No more tickets to mint."
        else do
            createTickets eventName (tail xs) eventTime
 
-- Mint the tickets 
-- Check that the deadline has not passed.
mintTicket :: TicketParams -> Contract w ProductSchema Text ()
mintTicket tParams = do
    -- Get the values from the ticket params
    let eventTime = tpDeadline tParams
    let ticketName = tpTicketName tParams
    -- Get the pub key hash from the current wallet
    pkh <- Contract.ownPaymentPubKeyHash
    -- Get the UTXOs at the address
    utxos <- utxosAt $ pubKeyHashAddress pkh Nothing
    now <- Contract.currentTime
    -- Check the deadline
    if now > eventTime
        then Contract.logError @String $ printf "Event has begun. Cannot sell anymore tickets. Current Time: %s. Event Time: %s" 
          (show now) 
          (show eventTime)
        else do
            logInfo @String $ printf "Before event. Current Time: %s. Event Time: %s" 
              (show now)          
              (show eventTime)            
            case Map.keys utxos of
                []       -> Contract.logError @String $ printf "No utxo found at this address: %s" (show pkh)
                oref : _ -> do
                    let currSymbol = (curSymbol pkh oref ticketName eventTime)
                    logInfo @String $ printf "Minting ticket. Name: %s. Currency Symbol: %s" (show ticketName) (show currSymbol)
                    let val     = Value.singleton currSymbol ticketName 1
                                 -- Set the minting policy
                        lookups = Constraints.mintingPolicy (policy pkh oref ticketName eventTime) <> 
                                  Constraints.unspentOutputs utxos
                                  -- Must mint the NFT
                        tx      = Constraints.mustMintValue val <>  
                                  -- Must spend the UTXO
                                  Constraints.mustSpendPubKeyOutput oref

                    ledgerTx <- submitTxConstraintsWith @Void lookups tx
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                    Contract.logInfo @String $ printf "Mint complete %s" (show val)
           

-- Buy a ticket based on the provided buy parameters
buyTicket :: BuyParams -> Contract w ProductSchema Text ()
buyTicket bp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let sellerAdd = bSellerAddress bp
    let ticketName = bToken bp
    Contract.logInfo @String $ printf "buyTicket - Trying to find sale. Buyer: %s" (show pkh)
    -- Find the ticket for sale
    sale <- findTicketSale (sellerAdd, bCurrSym bp, ticketName)
    case sale of
        Nothing -> Contract.logError @String $ printf "buyTicket - Could not locate the ticket requested: %s" (show ticketName)
        Just (oref, o, nfts) -> do            
            let r       = Redeemer $ PlutusTx.toBuiltinData Buy
                -- Value is the ticket NFT and the minimum ADA that must also be included with a token transaction
                valNFT   = Value.singleton (nCurrency nfts) (nToken nfts) 1 <> Ada.toValue Ledger.minAdaTxOut
                -- Price of the ticket
                valPrice = Ada.lovelaceValueOf (nPrice nfts)
                -- Just use min ADA amount as the fee.
                valFee = Ada.toValue Ledger.minAdaTxOut
            
            Contract.logInfo @String $ printf "buyTicket - Found ticket. Ticket: %s. Price: %s. Fee: %s" (show ticketName) (show valPrice) (show valFee)

            let lookups = Constraints.typedValidatorLookups (typedBuyValidator sellerAdd) <>
                          Constraints.unspentOutputs (Map.singleton oref o)    <>
                          Constraints.otherScript (buyValidator sellerAdd)
                tx      = Constraints.mustSpendScriptOutput oref r             <>
                          -- Must send the ticket to the buyer address
                          Constraints.mustPayToPubKey pkh valNFT               <>
                          -- Must send value of ticket price to seller
                          Constraints.mustPayToPubKey (nSeller nfts) valPrice  <> 
                          -- Must also send min tx fee
                          Constraints.mustPayToPubKey (pkh) valFee

            Contract.logInfo @String "Buy - Before submit transaction"
            ledgerTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String "Buy - Transaction confirmed"

-- Start a sale of event tickets
startSale :: StartParams -> Contract w ProductSchema Text ()
startSale sp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let tTokenName = sToken sp
    let tCurrSymbol = sCurrSym sp
    
    Contract.logInfo @String $ printf "startSale - Starting with Token: %s. Currency Symbol: %s" (show tTokenName) (show tCurrSymbol) 

    utxos <- utxosAt $ pubKeyHashAddress pkh Nothing
    if Map.null utxos
       then logInfo @String $ "startSale - No utxo's found"
    else do
        -- Must also transfter minimum ADA amount with token
        let valToken = Value.singleton (tCurrSymbol) (tTokenName) 1 <> Ada.toValue Ledger.minAdaTxOut
            -- Set the NFT datum that can be used when selling the ticket
            datum = TicketSale { nSeller = pkh, nToken = tTokenName, nCurrency = tCurrSymbol, nPrice = sPrice sp}
            lookups  = Constraints.unspentOutputs utxos <>
                      -- Set the buy validator
                       Constraints.typedValidatorLookups (typedBuyValidator pkh)
                      -- Lock the NFT with the script and datum
            tx       = Constraints.mustPayToTheScript datum valToken 
        Contract.logInfo @String $ printf "startSale - Value: %s" (show valToken)
        ledgerTx <- submitTxConstraintsWith @Sale lookups tx
        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
        Contract.logInfo @String "startSale transaction confirmed"

-- Find the the ticket based on the Currency Symbol and Token Name
findTicketSale :: (AsContractError e, ToJSON e) => (PaymentPubKeyHash, CurrencySymbol, TokenName) -> Contract w ProductSchema e (Maybe (TxOutRef, ChainIndexTxOut, TicketSale))
findTicketSale (sellerAdd, cs, tn) = do
    -- Get all UTXO's at the script address
    utxos <- Map.filter f <$> utxosTxOutTxAt (scriptAddress $ buyValidator sellerAdd)
    return $ case Map.toList utxos of
        [(oref, (o, citx))] -> do
            -- Try to retrieve the ticket datum
            nfts <- nftDatum (toTxOut o) $ \dh -> Map.lookup dh $ _citxData citx
            Just (oref, o, nfts)
        _           -> Nothing
  where
    -- Filter where the currency symbol and token name match the provided inputs
    f :: (ChainIndexTxOut, Plutus.ChainIndex.Tx.ChainIndexTx) -> Bool
    f (o, _) = valueOf (txOutValue $ toTxOut o) cs tn == 1


endpoints :: Contract () ProductSchema Text ()
endpoints = awaitPromise (createEvent' `select` buyTicket' `select` startSale') >> endpoints
 where
    createEvent' = endpoint @"createEvent" createEvent
    buyTicket'   = endpoint @"buyTicket" buyTicket
    startSale'   = endpoint @"startSale" startSale
