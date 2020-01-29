{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns
-fno-warn-name-shadowing
-fno-warn-unused-do-bind
-fno-warn-unused-top-binds #-}
module Spec.Marlowe.Marlowe2
    ( tests
    )
where

import           Test.Tasty

import           Language.Marlowe.Semantics
import           Language.Marlowe.Client2
import           Language.Plutus.Contract hiding (Contract)
import           Language.Plutus.Contract.Test
import           Language.PlutusTx.Lattice
import qualified Ledger
import qualified Ledger.Ada                                            as Ada
import           Ledger.Value                                          (TokenName, Value)


tests :: TestTree
tests = testGroup "token account"
    [ checkPredicate @MarloweSchema @ContractError "Create a Marlowe Contract" marloweContract2
        (assertNoFailedTransactions
        /\ assertNotDone w1 "contract should not have any errors"
        /\ walletFundsChange w1 (Ada.adaValueOf (-1))
        )
        (  callEndpoint @"create" w1 (Close) >> handleBlockchainEvents w1 )
    ]


w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
