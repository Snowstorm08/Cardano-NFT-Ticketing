{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DerivingStrategies  #-}


module Onchain
    ( 
      ticketMintingPolicy,
      curSymbol,
      policy,
      Sale,
      typedBuyValidator,
      buyValidator,
      buyValidatorHash,
      nftDatum
    )
    where

import           Codec.Serialise           (serialise)
import qualified Data.ByteString.Lazy      as BsLazy
import qualified Data.ByteString.Short     as BsShort
import qualified PlutusTx
import           Ledger.Ada                as Ada
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (mint, singleton)
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value (valueOf, flattenValue)
import qualified Plutus.V1.Ledger.Scripts  as Plutus
import qualified Plutus.V1.Ledger.Interval as LedgerIntervalV1
--import qualified Plutus.V2.Ledger.Contexts as Contexts
import           DataTypes


ticketMintingPolicy :: PaymentPubKeyHash -> TxOutRef -> TokenName -> POSIXTime -> () -> ScriptContext -> Bool
ticketMintingPolicy pkh oref tn deadline () ctx = traceIfFalse "Minting Policy - UTxO not consumed"   hasUTxO  &&
                                               traceIfFalse "Minting Policy - Incorrect signature" signedByOwner &&
                                               traceIfFalse "Minting Policy - Incorrect mint amount. Must be 1" checkMintedAmount &&
                                               traceIfFalse "Minting Policy - After deadline" isBeforeDeadline
   where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    -- Check the transaction is signed by the correct owner
    signedByOwner :: Bool
    signedByOwner = txSignedBy txInfo $ unPaymentPubKeyHash pkh

    -- Check that the deadline POSIX time is after the current POSIX time
    isBeforeDeadline :: Bool
    isBeforeDeadline = not $ LedgerIntervalV1.contains (LedgerIntervalV1.from deadline) (txInfoValidRange txInfo)

    -- Check the UTXO is consumed                 
    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs txInfo

    -- Check the single mint amount
    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint txInfo) of
        [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tn && amt == 1
        _                -> False

policy :: PaymentPubKeyHash -> TxOutRef -> TokenName -> POSIXTime -> Scripts.MintingPolicy
policy pkh oref tn deadline = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \pkh' oref' tn' deadline' -> Scripts.wrapMintingPolicy $ ticketMintingPolicy pkh' oref' tn' deadline' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode deadline

{-# INLINABLE mkBuyValidator #-}
mkBuyValidator :: TicketSale -> SaleAction -> ScriptContext -> Bool
mkBuyValidator nfts r ctx = case r of
    Buy     -> traceIfFalse "Invalid signature. Expecting just 1 signature." valiSig &&
               traceIfFalse "Ticket was not transferred" (valueOf (valuePaidTo info getSig) (nCurrency nfts) (nToken nfts) == 1) && 
               traceIfFalse "Incorrect price paid" (checkPrice (unPaymentPubKeyHash (nSeller nfts)) (nPrice nfts))
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    valiSig :: Bool
    valiSig = length(txInfoSignatories info) == 1

    getSig :: PubKeyHash
    getSig = PlutusTx.Prelude.head (txInfoSignatories info)

    -- Check the correct price has been paid to the seller
    checkPrice :: PubKeyHash -> Integer -> Bool
    checkPrice seller price = fromInteger (Ada.getLovelace (Ada.fromValue (valuePaidTo info seller))) >= fromInteger price

data Sale
instance Scripts.ValidatorTypes Sale where
    type instance DatumType Sale    = TicketSale
    type instance RedeemerType Sale = SaleAction

typedBuyValidator :: PaymentPubKeyHash -> Scripts.TypedValidator Sale
typedBuyValidator mp = Scripts.mkTypedValidator @Sale
    ($$(PlutusTx.compile [|| mkBuyValidator ||]))
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @TicketSale @SaleAction

{-# INLINABLE nftDatum #-}
nftDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe TicketSale
nftDatum o f = do
    dh <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

curSymbol :: PaymentPubKeyHash -> TxOutRef -> TokenName -> POSIXTime -> CurrencySymbol
curSymbol pkh oref tn deadline = scriptCurrencySymbol $ policy pkh oref tn deadline

buyValidator :: PaymentPubKeyHash -> Validator
buyValidator = Scripts.validatorScript . typedBuyValidator

buyValidatorHash :: PaymentPubKeyHash -> ValidatorHash
buyValidatorHash = validatorHash . buyValidator

buyScript :: PaymentPubKeyHash -> Plutus.Script
buyScript = Ledger.unValidatorScript . buyValidator

buyScriptAsShortBs :: PaymentPubKeyHash -> BsShort.ShortByteString
buyScriptAsShortBs = BsShort.toShort . BsLazy.toStrict . serialise . buyScript
