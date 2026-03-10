{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}

module Onchain
    ( ticketMintingPolicy
    , curSymbol
    , policy
    , Sale
    , typedBuyValidator
    , buyValidator
    , buyValidatorHash
    , nftDatum
    ) where

import           Codec.Serialise           (serialise)
import qualified Data.ByteString.Lazy      as BsLazy
import qualified Data.ByteString.Short     as BsShort
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)

import           Ledger                    hiding (singleton)
import           Ledger.Ada                as Ada
import           Ledger.Value              (valueOf, flattenValue)

import qualified Ledger.Typed.Scripts      as Scripts
import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Scripts  as Plutus

import           DataTypes

--------------------------------------------------------------------------------
-- Minting Policy
--------------------------------------------------------------------------------

{-# INLINABLE ticketMintingPolicy #-}
ticketMintingPolicy :: PaymentPubKeyHash -> TxOutRef -> TokenName -> POSIXTime -> () -> ScriptContext -> Bool
ticketMintingPolicy pkh oref tn deadline _ ctx =
       traceIfFalse "UTxO not consumed" hasUTxO
    && traceIfFalse "Missing owner signature" signedByOwner
    && traceIfFalse "Invalid mint amount" checkMint
    && traceIfFalse "Mint after deadline" beforeDeadline
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByOwner :: Bool
    signedByOwner =
        txSignedBy info (unPaymentPubKeyHash pkh)

    beforeDeadline :: Bool
    beforeDeadline =
        not $ Interval.contains (Interval.from deadline) (txInfoValidRange info)

    hasUTxO :: Bool
    hasUTxO =
        any (\i -> txInInfoOutRef i == oref) (txInfoInputs info)

    checkMint :: Bool
    checkMint =
        case flattenValue (txInfoMint info) of
            [(cs, tn', amt)] ->
                   cs == ownCurrencySymbol ctx
                && tn' == tn
                && amt == 1
            _ -> False


policy :: PaymentPubKeyHash -> TxOutRef -> TokenName -> POSIXTime -> Scripts.MintingPolicy
policy pkh oref tn deadline =
    mkMintingPolicyScript $
        $$(PlutusTx.compile
            [|| \pkh' oref' tn' deadline' ->
                    Scripts.wrapMintingPolicy
                        (ticketMintingPolicy pkh' oref' tn' deadline')
            ||])
        `PlutusTx.applyCode` PlutusTx.liftCode pkh
        `PlutusTx.applyCode` PlutusTx.liftCode oref
        `PlutusTx.applyCode` PlutusTx.liftCode tn
        `PlutusTx.applyCode` PlutusTx.liftCode deadline


--------------------------------------------------------------------------------
-- Buy Validator
--------------------------------------------------------------------------------

{-# INLINABLE mkBuyValidator #-}
mkBuyValidator :: TicketSale -> SaleAction -> ScriptContext -> Bool
mkBuyValidator sale action ctx =
    case action of
        Buy ->
               traceIfFalse "Invalid signature" singleSig
            && traceIfFalse "Ticket not transferred" ticketTransferred
            && traceIfFalse "Incorrect price" correctPrice
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    sigs :: [PubKeyHash]
    sigs = txInfoSignatories info

    singleSig :: Bool
    singleSig =
        case sigs of
            [_] -> True
            _   -> False

    buyer :: PubKeyHash
    buyer =
        case sigs of
            [s] -> s
            _   -> traceError "signature"

    ticketTransferred :: Bool
    ticketTransferred =
        valueOf
            (valuePaidTo info buyer)
            (nCurrency sale)
            (nToken sale)
        == 1

    correctPrice :: Bool
    correctPrice =
        let paid =
                Ada.getLovelace $
                Ada.fromValue $
                valuePaidTo info (unPaymentPubKeyHash (nSeller sale))
        in paid >= nPrice sale


--------------------------------------------------------------------------------
-- Typed Validator
--------------------------------------------------------------------------------

data Sale

instance Scripts.ValidatorTypes Sale where
    type instance DatumType Sale    = TicketSale
    type instance RedeemerType Sale = SaleAction


typedBuyValidator :: PaymentPubKeyHash -> Scripts.TypedValidator Sale
typedBuyValidator _ =
    Scripts.mkTypedValidator @Sale
        $$(PlutusTx.compile [|| mkBuyValidator ||])
        $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @TicketSale @SaleAction


--------------------------------------------------------------------------------
-- Datum helper
--------------------------------------------------------------------------------

{-# INLINABLE nftDatum #-}
nftDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe TicketSale
nftDatum o f = do
    dh <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

curSymbol :: PaymentPubKeyHash -> TxOutRef -> TokenName -> POSIXTime -> CurrencySymbol
curSymbol pkh oref tn deadline =
    scriptCurrencySymbol (policy pkh oref tn deadline)

buyValidator :: PaymentPubKeyHash -> Validator
buyValidator =
    Scripts.validatorScript . typedBuyValidator

buyValidatorHash :: PaymentPubKeyHash -> ValidatorHash
buyValidatorHash =
    validatorHash . buyValidator


buyScript :: PaymentPubKeyHash -> Plutus.Script
buyScript =
    Ledger.unValidatorScript . buyValidator


buyScriptAsShortBs :: PaymentPubKeyHash -> BsShort.ShortByteString
buyScriptAsShortBs =
      BsShort.toShort
    . BsLazy.toStrict
    . serialise
    . buyScript
