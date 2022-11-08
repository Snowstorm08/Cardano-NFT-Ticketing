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
import           Control.Lens
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
    .\/ Endpoint "startSaleAllTickets" StartSaleParams
    .\/ Endpoint "buyTicketByName" BuyTicketByNameParams

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
        then Contract.logInfo @String $ printf "Event has begun. Cannot sell anymore tickets. Current Time: %s. Event Time: %s" 
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
    Contract.logInfo @String $ printf "buyTicket - Trying to find sale. Buyer: %s. Seller: %s." (show pkh) (show sellerAdd)
    -- Find the ticket for sale
    sale <- findTicketSale (sellerAdd, bCurrSym bp, ticketName)
    case sale of
        Nothing -> Contract.logError @String $ printf "buyTicket - Could not locate the ticket requested: %s. Currency Symbol: %s." (show ticketName) (show $ bCurrSym bp)
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
startSale :: Integer -> CurrencySymbol -> TokenName -> Contract w ProductSchema Text ()
startSale tPrice tCurrSymbol tTokenName = do
    pkh <- Contract.ownPaymentPubKeyHash  
    Contract.logInfo @String $ printf "startSale - Starting with Token: %s. Currency Symbol: %s" (show tTokenName) (show tCurrSymbol) 

    utxos <- utxosAt $ pubKeyHashAddress pkh Nothing
    if Map.null utxos
       then logInfo @String $ "startSale - No utxo's found"
    else do
        -- Must also transfter minimum ADA amount with token
        let valToken = Value.singleton (tCurrSymbol) (tTokenName) 1 <> Ada.toValue Ledger.minAdaTxOut
            -- Set the NFT datum that can be used when selling the ticket
            datum = TicketSale { nSeller = pkh, nToken = tTokenName, nCurrency = tCurrSymbol, nPrice = tPrice}
            lookups  = Constraints.unspentOutputs utxos <>
                      -- Set the buy validator
                       Constraints.typedValidatorLookups (typedBuyValidator pkh)
                      -- Lock the NFT with the script and datum
            tx       = Constraints.mustPayToTheScript datum valToken 
        Contract.logInfo @String $ printf "startSale - Value: %s" (show valToken)
        ledgerTx <- submitTxConstraintsWith @Sale lookups tx
        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
        Contract.logInfo @String "startSale transaction confirmed"

buyTicketByName :: BuyTicketByNameParams -> Contract w ProductSchema Text ()
buyTicketByName bp = do
   let bSellerAdd = btSellerAddress bp
   let bTicketName = btTicketName bp
   Contract.logInfo @String $ printf "buyTicketByName - Trying to buy ticket: %s. From address: %s." (show bTicketName) (show bSellerAdd) 
   utxos <- utxosAt $ (scriptAddress $ buyValidator bSellerAdd)
   let utxosFlat = Value.flattenValue $ mconcat . map (^. ciTxOutValue) . Map.elems $ utxos
   if length utxosFlat > 0 then
      tryBuyTicket bp utxosFlat
    else
      Contract.logError @String $ printf "buyTicketByName. No UTXO's found"

tryBuyTicket :: BuyTicketByNameParams -> [(CurrencySymbol, TokenName, Integer)] -> Contract w ProductSchema Text ()
tryBuyTicket bp xs = do
   let utxo = head xs
   let utxoRemaining = tail xs
   let bpTicketRequested = fromString (btTicketName bp)

    -- Extract the values we need based on their index
   let cs = utxo ^. _1
   let tname = utxo ^. _2
   let tamount = utxo ^. _3

   if tname == bpTicketRequested && tamount == 1 then do
     let tTicket = BuyParams cs tname (btSellerAddress bp)
     logInfo @String $ printf "tryBuyTicket. Found ticket to buy. Token Name: %s. Currency Symbol: %s." (show tname) (show cs)
     buyTicket tTicket
   else do
    if length utxoRemaining == 0 then 
      logInfo @String $ "tryBuyTicket. No more tickets found."
    else
      tryBuyTicket bp utxoRemaining

startSaleAllTickets :: StartSaleParams -> Contract w ProductSchema Text ()
startSaleAllTickets sp = do
    let ticketPrice = saPrice sp
    pkh <- Contract.ownPaymentPubKeyHash
    Contract.logInfo @String $ printf "startSaleAllTickets - Starting sale of all tickets at address: %s. Price: %s" (show pkh) (show ticketPrice)
    utxos <- utxosAt $ pubKeyHashAddress pkh Nothing
    let utxosFlat = Value.flattenValue $ mconcat . map (^. ciTxOutValue) . Map.elems $ utxos
    Contract.logInfo @String $ printf "startSaleAllTickets. UTXO's %s" (show utxosFlat)
    tryStartSale sp utxosFlat

tryStartSale :: StartSaleParams -> [(CurrencySymbol, TokenName, Integer)] -> Contract w ProductSchema Text ()
tryStartSale sp xs = do
    let utxo = head xs
    let utxoRemaining = tail xs

    -- Extract the values we need based on their index
    let cs = utxo ^. _1
    let tname = utxo ^. _2
    let tamount = utxo ^. _3

    if cs /= "" && tname /= "" && tamount == 1 then do
      Contract.logInfo @String $ printf "tryStartSale. Currency Symbol: %s. Token Name: %s. Amount: %s" (show cs) (show tname) (show tamount)
      startSale (saPrice sp) cs tname
    else do
      Contract.logInfo @String $ printf "tryStartSale. Skipped UTXO, not a valid ticket. Token Name: %s" (show tname) 

    if length utxoRemaining == 0 then 
      logInfo @String $ "tryStartSale. No more tickets to put on sale."
    else
      tryStartSale sp utxoRemaining

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
endpoints = awaitPromise (createEvent' `select` startSaleAllTickets' `select` buyTicketByName') >> endpoints
 where
    createEvent' = endpoint @"createEvent" createEvent
    buyTicketByName'   = endpoint @"buyTicketByName" buyTicketByName
    startSaleAllTickets' = endpoint @"startSaleAllTickets" startSaleAllTickets