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
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (maybeToList)
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import           Language.Plutus.Contract
import qualified Language.Plutus.Contract.Tx  as Tx
import           Language.Marlowe.Semantics (Input, MarloweData(..), emptyState, marloweValidator)
import qualified Language.Marlowe.Semantics as Marlowe
import qualified Language.PlutusTx          as PlutusTx
import qualified Language.PlutusTx.Prelude  as P
import           Ledger                     (DataValue (..), PubKeyHash (..), pubKeyHash, Slot (..), Tx, TxOut (..), interval,

                                             mkValidatorScript, pubKeyHashTxOut, scriptAddress, scriptTxIn, scriptTxOut,
                                             txOutRefs)
import           Ledger.Ada                 (adaValueOf)
import           Ledger.Scripts             (RedeemerValue (..), Validator)
import qualified Ledger.Typed.Scripts       as Scripts
import qualified Ledger.Value               as Val

type MarloweSchema =
    BlockchainActions
        .\/ Endpoint "create" Marlowe.Contract


marloweContract2 :: forall e. (AsContractError e)
    => Contract MarloweSchema e ()
marloweContract2 = do
    cont <- endpoint @"create" @Marlowe.Contract @MarloweSchema
    createContract cont

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

        marloweData = MarloweData {
            marloweCreator = creator,
            marloweContract = contract,
            marloweState = emptyState slot }
        ds = DataValue $ PlutusTx.toData marloweData

    let payValue = adaValueOf 1

    let slotRange = interval slot (slot + 10)
    let tx = payToScript payValue (Ledger.scriptAddress validator) ds
             <> mustBeValidIn slotRange
    void $ submitTx tx


{-| Generate a validator script for 'creator' PubKey -}
validatorScript :: PubKeyHash -> Validator
validatorScript creator = mkValidatorScript ($$(PlutusTx.compile [|| validatorParam ||])
    `PlutusTx.applyCode`
        PlutusTx.liftCode creator)
    where validatorParam k = Scripts.wrapValidator (marloweValidator k)


{-| Make redeemer script -}
mkRedeemer :: [Input] -> RedeemerValue
mkRedeemer inputs = RedeemerValue (PlutusTx.toData inputs)
