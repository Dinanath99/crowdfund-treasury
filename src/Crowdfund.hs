{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Crowdfund where

import PlutusTx
import PlutusTx.Prelude
import PlutusLedgerApi.V3 
    ( PubKeyHash
    , Lovelace(..)
    , POSIXTime
    , POSIXTimeRange
    , TxOutRef
    , TxOut
    , TxInInfo(..)
    , ScriptContext(..)
    , TxInfo(..)
    , txOutValue
    , txInInfoResolved
    , Value
    , adaSymbol
    , adaToken
    )
import PlutusLedgerApi.V1.Interval (contains, to, from)
import PlutusLedgerApi.V1.Value (valueOf)
import PlutusLedgerApi.Common (serialiseCompiledCode)
import Cardano.Api.Shelley (PlutusScript, PlutusScriptV3, PlutusScript(PlutusScriptSerialised))
import PlutusCore.Builtin.Debug (plcVersion110)
import GHC.Generics (Generic)
import Prelude (Show)

data CrowdfundDatum = CrowdfundDatum
    { goal :: Lovelace
    , deadline :: POSIXTime
    , creator :: PubKeyHash
    } deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''CrowdfundDatum
PlutusTx.makeLift ''CrowdfundDatum

data CrowdfundRedeemer = CollectFunds | Refund
    deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''CrowdfundRedeemer

{-# INLINABLE mkValidator #-}
mkValidator :: CrowdfundDatum -> CrowdfundRedeemer -> () -> ScriptContext -> Bool
mkValidator datum red _ ctx =
    let info = scriptContextTxInfo ctx
        -- Calculate total lovelace from all inputs
        inputValues = [txOutValue (txInInfoResolved txIn) | txIn <- txInfoInputs info]
        totalLovelace = sum [valueOf v adaSymbol adaToken | v <- inputValues]
        currentTime = txInfoValidRange info
        Lovelace goalAmount = goal datum
    in case red of
        CollectFunds ->
            traceIfFalse "Funding goal not met" (totalLovelace >= goalAmount) &&
            traceIfFalse "Deadline has passed" (contains (to (deadline datum)) currentTime) &&
            traceIfFalse "Not signed by creator" (ownInputSignedBy (creator datum) info)
        Refund ->
            traceIfFalse "Deadline not passed" (not (contains (to (deadline datum)) currentTime))
  where
    ownInputSignedBy pkh info = pkh `elem` txInfoSignatories info

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: CrowdfundDatum -> BuiltinData -> BuiltinData -> BuiltinUnit
mkValidatorUntyped params red ctx = 
    check (mkValidator params (unsafeFromBuiltinData red) () (unsafeFromBuiltinData ctx))

cmpCode :: CrowdfundDatum -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
cmpCode param =
    $$(compile [|| mkValidatorUntyped ||]) `unsafeApplyCode` liftCode plcVersion110 param

validatorSerialized :: CrowdfundDatum -> PlutusScript PlutusScriptV3
validatorSerialized param = PlutusScriptSerialised (serialiseCompiledCode (cmpCode param))