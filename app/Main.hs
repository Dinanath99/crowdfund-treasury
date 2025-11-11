

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DataKinds #-}

module Main where

import PlutusTx
import Cardano.Api
import Cardano.Kuber.Api
import Cardano.Kuber.Util
import Cardano.Api.Shelley (PlutusScript, PlutusScript(..), PlutusScriptV3)
import PlutusLedgerApi.V3 (PubKeyHash(..), Lovelace(..), POSIXTime(..), toBuiltin)
import Data.Text (pack, unpack)
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import System.Environment (getArgs)
import Crowdfund (CrowdfundDatum(..), validatorSerialized, CrowdfundRedeemer(..))

signingKeyToPKH :: SigningKey PaymentKey -> PubKeyHash
signingKeyToPKH sk =
    let vkey = getVerificationKey sk
        bs = serialiseToRawBytes (verificationKeyHash vkey)
    in PubKeyHash (toBuiltin bs)

parseAddressString :: String -> Either FrameworkError (AddressInEra ConwayEra)
parseAddressString addr =
    case deserialiseAddress (AsAddressInEra AsConwayEra) (pack addr) of
        Just addr -> Right addr
        Nothing -> Left $ FrameworkError ParserError "Failed to parse address"

fundScript
    :: CrowdfundDatum
    -> AddressInEra ConwayEra
    -> Value
    -> SigningKey PaymentKey
    -> Kontract ChainConnectInfo w FrameworkError TxBuilder
fundScript datum scriptAddr value walletSkey = do
    let scriptDatum = unsafeHashableScriptData $ fromPlutusData $ toData datum
    pure $ txPayToScriptWithData scriptAddr value scriptDatum
           <> txWalletSignKey walletSkey

collectFromScript
    :: PlutusScript PlutusScriptV3
    -> AddressInEra ConwayEra
    -> SigningKey PaymentKey
    -> AddressInEra ConwayEra
    -> Kontract ChainConnectInfo w FrameworkError TxBuilder
collectFromScript script scriptAddr creatorSkey collectorAddr = do
    (UTxO contractUTxos) <- kQueryUtxoByAddress (Set.singleton (addressInEraToAddressAny scriptAddr))
    if Map.null contractUTxos
        then kError LibraryError "No UTxOs found at script address"
        else do
            let redeemer = unsafeHashableScriptData $ fromPlutusData $ toData CollectFunds
            pure $ mconcat [ txRedeemUtxo txIn txOut script redeemer Nothing
                           | (txIn, txOut) <- Map.toList contractUTxos ]
                   <> txChangeAddress collectorAddr
                   <> txWalletSignKey creatorSkey

refundFromScript
    :: PlutusScript PlutusScriptV3
    -> AddressInEra ConwayEra
    -> SigningKey PaymentKey
    -> AddressInEra ConwayEra
    -> Kontract ChainConnectInfo w FrameworkError TxBuilder
refundFromScript script scriptAddr walletSkey refundAddr = do
    (UTxO contractUTxos) <- kQueryUtxoByAddress (Set.singleton (addressInEraToAddressAny scriptAddr))
    if Map.null contractUTxos
        then kError LibraryError "No UTxOs found at script address"
        else do
            let redeemer = unsafeHashableScriptData $ fromPlutusData $ toData Refund
            pure $ mconcat [ txRedeemUtxo txIn txOut script redeemer Nothing
                           | (txIn, txOut) <- Map.toList contractUTxos ]
                   <> txChangeAddress refundAddr
                   <> txWalletSignKey walletSkey

main :: IO ()
main = do
    args <- getArgs
    conn <- chainInfoFromEnv

    creatorSkey <- readSignKey "./test-keys/alice.skey"
    funder1Skey <- readSignKey "./test-keys/bob.skey"
    funder2Skey <- readSignKey "./test-keys/ding.skey"

    let creatorPkh = signingKeyToPKH creatorSkey
        network = Testnet (NetworkMagic 2)  -- Preview testnet
        -- Set deadline to ~1 hour from now (3600 seconds * 1000 = 3,600,000 ms)
        -- Adjust based on current POSIXTime from cardano-cli query tip
        crowdParam = CrowdfundDatum
            { goal = Lovelace 5_000_000  -- 5 ADA
            , deadline = POSIXTime 1731250800000  -- Nov 10, 2025, + 1 hour
            , creator = creatorPkh
            }
        script = validatorSerialized crowdParam
        scriptHash = hashScript (PlutusScript PlutusScriptV3 script)
        scriptAddr = makeShelleyAddressInEra shelleyBasedEra network (PaymentCredentialByScript scriptHash) NoStakeAddress

    case args of
        ["check"] -> putStrLn "Crowdfund setup ready!"

        ["cbor-writer"] -> do
            let description = "Crowdfunding validator script (V3)."
            errOrUnit <- writeFileTextEnvelope (File "Crowdfund.plutus") (Just description) script
            case errOrUnit of
                Left err -> print err
                Right () -> putStrLn "Wrote script to Crowdfund.plutus"

        ["sAddr"] -> do
            putStrLn $ "Script Address: " ++ T.unpack (serialiseAddress scriptAddr)

        ["fund"] -> do
            putStrLn $ "Funding at: " ++ T.unpack (serialiseAddress scriptAddr)
            let val = lovelaceToValue 3_000_000  -- 3 ADA
            result <- evaluateKontract conn $ do
                txb <- fundScript crowdParam scriptAddr val funder1Skey
                kBuildAndSubmit txb
            case result of
                Left e -> putStrLn $ "Error funding: " ++ show e
                Right _ -> putStrLn "Funding successful!"

        ["fund2"] -> do
            putStrLn $ "Funding at: " ++ T.unpack (serialiseAddress scriptAddr)
            let val = lovelaceToValue 3_000_000  -- 3 ADA
            result <- evaluateKontract conn $ do
                txb <- fundScript crowdParam scriptAddr val funder2Skey
                kBuildAndSubmit txb
            case result of
                Left e -> putStrLn $ "Error funding: " ++ show e
                Right _ -> putStrLn "Funding successful!"

        ["collect"] -> do
            collectorAddr <- case parseAddressString "addr_test1wzk2nn0uqw0d024q5k5a4mlafkt6nd0jt8lf88jrgx6ehmqejwreu" of
                Left e -> fail $ "Invalid address: " ++ show e
                Right a -> pure a
            result <- evaluateKontract conn $ do
                txb <- collectFromScript script scriptAddr creatorSkey collectorAddr
                kBuildAndSubmit txb
            case result of
                Left e -> putStrLn $ "Error collecting: " ++ show e
                Right _ -> putStrLn "Collection successful!"

        ["refund"] -> do
            refundAddr <- case parseAddressString "addr_test1vp8cwrmca7u87p0sf3ermrqx765rk24lw2avv4prs3vhslc8emn7t" of
                Left e -> fail $ "Invalid address: " ++ show e
                Right a -> pure a
            result <- evaluateKontract conn $ do
                txb <- refundFromScript script scriptAddr funder1Skey refundAddr
                kBuildAndSubmit txb
            case result of
                Left e -> putStrLn $ "Error refunding: " ++ show e
                Right _ -> putStrLn "Refund successful!"

        _ -> putStrLn "Invalid command. Use: check, cbor-writer, sAddr, fund, fund2, collect, refund"