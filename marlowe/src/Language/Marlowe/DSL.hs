{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
-- Big hammer, but helps
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Language.Marlowe.DSL where

import           GHC.Generics               (Generic)
-- import           Control.Monad.Cont
import Control.Monad.State.Lazy as St
import           Language.Marlowe.Semantics hiding (State)
import           Language.Marlowe.Util
import           Language.PlutusTx          (makeIsData)
import qualified Language.PlutusTx          as PlutusTx
import           Language.PlutusTx.AssocMap (Map)
import qualified Language.PlutusTx.AssocMap as Map
import           Language.PlutusTx.Lift     (makeLift)
import           Language.PlutusTx.Prelude  hiding ((<>))
import           Ledger                     (PubKeyHash (..), Slot (..), ValidatorHash)
import           Ledger.Interval            (Extended (..), Interval (..), LowerBound (..), UpperBound (..))
import           Ledger.Scripts             (DataValue (..))
import           Ledger.Validation
import           Ledger.Value               (CurrencySymbol, TokenName)
import qualified Ledger.Value               as Val
import qualified Prelude                    as P
import           Text.PrettyPrint.Leijen    (comma, hang, lbrace, line, rbrace, space, text, (<>))

class Slotable s where
    toSlot :: s -> Slot

instance (P.Integral s) => Slotable s where
    toSlot s = Slot (toInteger s)

class IsContract s where
    toContract :: s -> Contract

instance IsContract Contract where
    toContract = id

instance IsContract Cont where
    toContract = fromCont

class IsValue v where
    toValue :: v -> Value

instance (P.Integral s) => IsValue s where
    toValue s = Constant (toInteger s)


before :: (Slotable s) => s -> [Case Contract] -> Cont
before s cases = continue $ When cases (toSlot s)

after :: (Slotable s) => s -> Cont
after s = before s []


deposit :: (IsContract c) => Party -> Party -> Integer -> c -> Case Contract
deposit from to val cont = Case (Deposit (AccountId 0 to) from ada (Constant val)) (toContract cont)

zcb :: Party -> Party -> Integer -> Integer -> Contract
zcb investor issuer notional discount = fromCont $ do
    before 100 [deposit investor investor (notional - discount) $ do
        payAllAda investor issuer
        before 200 [deposit issuer investor notional Close]
        ]


escrow alice bob = before 10 [
    deposit alice alice 450 $ do
        before 50 [chooseBit alice "a" (const $ payAllAda alice bob)]
        ]

if' :: (IsContract c) => Observation -> c -> Cont
if' obs tr = continue $ If obs (toContract tr)

choose :: (IsContract c) => [Bound] -> Party -> ByteString -> (ChoiceId -> c) -> Case Contract
choose bounds party name f = let
    choiceId = ChoiceId name party
    in Case (Choice choiceId bounds) (toContract (f choiceId))

chooseRange l h = choose [Bound l h]
chooseBit = chooseRange 0 1
chooseZero = choose [Bound 0 0]
chooseOne = choose [Bound 1 1]
-- pay :: Party -> Party -> (Contract -> Contract) -> Cont Contract Contract
-- pay from to = \c -> prev $ Pay (AccountId 0 from) (Party to) ada (AvailableMoney (AccountId 0 from) ada) c
type Cont = State (Contract -> Contract) ()

continue :: (Contract -> Contract) -> Cont
continue contract = do
    modify $ \currCont cont -> currCont $ contract cont
    return ()

close :: Cont
close = continue $ const Close

pay :: (IsValue v) => Party -> Party -> Token -> v -> Cont
pay from to tok val = continue $ Pay (AccountId 0 from) (Party to) tok (toValue val)

payAll :: Party -> Party -> Token -> Cont
payAll from to tok = continue $ Pay (AccountId 0 from) (Party to) tok (AvailableMoney (AccountId 0 from) tok)

payAda :: Integral v => Party -> Party -> v -> Cont
payAda from to val = pay from to ada val

payAllAda :: Party -> Party -> Cont
payAllAda from to = payAll from to ada

fromCont :: State (Contract -> Contract) a -> Contract
fromCont cont = execState cont id Close

infix 4 ===
infix 3 &&&
infix 2 |||
(===) :: Value -> Value -> Observation
a === b = ValueEQ a b
(&&&) :: Observation -> Observation -> Observation
a &&& b = AndObs a b
(|||) :: Observation -> Observation -> Observation
a ||| b = OrObs a b


