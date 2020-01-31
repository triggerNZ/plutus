{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Language.Marlowe.Client2 where
import           Control.Monad              (Monad (..), void)
import           Control.Monad.Error.Class  (MonadError (..))
import           Control.Monad.Error.Lens   (throwing)
import           Control.Lens
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (maybeToList)
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import           Language.Plutus.Contract
import qualified Language.Plutus.Contract.Tx  as Tx
import           Language.Marlowe.Semantics hiding (Contract)
import qualified Language.Marlowe.Semantics as Marlowe
import qualified Language.PlutusTx          as PlutusTx
import qualified Language.PlutusTx.Prelude  as P
import           Ledger                     (DataValue (..), PubKeyHash (..), pubKeyHash, Slot (..), Tx, TxOut (..), TxOutTx (..), interval,

                                             mkValidatorScript, pubKeyHashTxOut, scriptAddress, scriptTxIn, scriptTxOut,
                                             txOutRefs, txOutTxData)
import           Ledger.Ada                 (adaValueOf)
import           Ledger.AddressMap                 (outRefMap)
import           Ledger.Scripts             (RedeemerValue (..), Validator)
import qualified Ledger.Typed.Scripts       as Scripts
import qualified Ledger.Value               as Val

type MarloweSchema =
    BlockchainActions
        .\/ Endpoint "create" Marlowe.Contract
        .\/ Endpoint "apply-inputs" (PubKeyHash, [Input])


marloweContract2 :: forall e. (AsContractError e)
    => Contract MarloweSchema e ()
marloweContract2 = do
    create <|> apply
  where
    create = do
        cont <- endpoint @"create" @Marlowe.Contract @MarloweSchema
        createContract cont
    apply = do
        (creator, inputs) <- endpoint @"apply-inputs" @(PubKeyHash, [Input]) @MarloweSchema
        MarloweData{..} <- applyInputs creator inputs
        case marloweContract of
            Close -> pure ()
            _ -> void apply


{-| Create a Marlowe contract.
    Uses wallet public key to generate a unique script address.
 -}
createContract :: (AsContractError e)
    => Marlowe.Contract
    -> Contract MarloweSchema e ()
createContract contract = do
    slot <- awaitSlot 0
    creator <- pubKeyHash <$> ownPubKey
    let validator = validatorScript creator
        address = Ledger.scriptAddress validator

        marloweData = MarloweData {
            marloweCreator = creator,
            marloweContract = contract,
            marloweState = emptyState slot }
        ds = DataValue $ PlutusTx.toData marloweData

    let payValue = adaValueOf 1

    let slotRange = interval slot (slot + 10)
    let tx = payToScript payValue address ds
             <> mustBeValidIn slotRange
    -- void $ watchAddressUntil address (Slot $ contractLifespanUpperBound contract)
    void $ submitTx tx


applyInputs :: (AsContractError e)
    => PubKeyHash
    -> [Input]
    -> Contract MarloweSchema e MarloweData
applyInputs creator inputs = do
    let redeemer = mkRedeemer inputs
        validator = validatorScript creator
        address = scriptAddress validator
    slot <- awaitSlot 0

    utxo <- watchAddressUntil address (Slot 1000)
    let [(ref, out)] = Map.toList (outRefMap utxo)


    let convert :: TxOutRef -> TxOutTx -> Maybe (TxOutRef, Val.Value, DataValue, MarloweData)
        convert ref out = case txOutTxData out of
            Just dv -> case PlutusTx.fromData (getDataScript dv) of
                Just marloweData -> Just (ref, txOutValue (txOutTxOut out), dv, marloweData)
                _ -> Nothing
            _ -> Nothing

    (ref, value, dataValue, marloweData@MarloweData{..}) <- case convert ref out of
        Just r -> pure r
        Nothing -> throwing _OtherError $ (Text.pack $ "Not found")

    -- For now, we expect a transaction to happen whithin 10 slots from now.
    -- That's about 3 minutes, should be fine.
    let slotRange = interval slot (slot + Slot 10)
    let txInput = TransactionInput {
            txInterval = (slot, slot + Slot 10),
            txInputs = inputs }

    let computedResult = computeTransaction txInput marloweState marloweContract

    (tx, md) <- case computedResult of
        TransactionOutput {txOutPayments, txOutState, txOutContract} -> do

            let marloweData = MarloweData {
                    marloweCreator,
                    marloweContract = txOutContract,
                    marloweState = txOutState }

            let deducedTxOutputs = case txOutContract of
                    Close -> txPaymentOuts txOutPayments
                    _ -> let
                        txWithPayouts = txPaymentOuts txOutPayments
                        totalPayouts = foldMap (\(Payment _ v) -> v) txOutPayments
                        finalBalance = totalIncome P.- totalPayouts
                        dataValue = DataValue (PlutusTx.toData marloweData)
                        scriptOut = payToScript finalBalance address dataValue
                        in scriptOut <> txWithPayouts

            return (deducedTxOutputs, marloweData)
        Error txError -> throwing _OtherError $ (Text.pack $ show txError)

    let scriptIn = scriptTxIn ref validator redeemer dataValue
    let finalTx = tx <> mustSpendInput scriptIn <> mustBeValidIn slotRange
    void $ submitTx finalTx
    pure marloweData
  where
    collectDeposits (IDeposit _ _ (Token cur tok) amount) = Val.singleton cur tok amount
    collectDeposits _                    = P.zero

    totalIncome = foldMap collectDeposits inputs

    txPaymentOuts :: [Payment] -> UnbalancedTx
    txPaymentOuts payments = let
        ps = foldr collectPayments Map.empty payments
        in foldMap (\(pk, value) -> payToPubKeyHash value pk) (Map.toList ps)

    collectPayments :: Payment -> Map Party Money -> Map Party Money
    collectPayments (Payment party money) payments = let
        newValue = case Map.lookup party payments of
            Just value -> value P.+ money
            Nothing    -> money
        in Map.insert party newValue payments


{-| Generate a validator script for 'creator' PubKey -}
validatorScript :: PubKeyHash -> Validator
validatorScript creator = mkValidatorScript ($$(PlutusTx.compile [|| validatorParam ||])
    `PlutusTx.applyCode`
        PlutusTx.liftCode creator)
    where validatorParam k = Scripts.wrapValidator (marloweValidator k)


{-| Make redeemer script -}
mkRedeemer :: [Input] -> RedeemerValue
mkRedeemer inputs = RedeemerValue (PlutusTx.toData inputs)
