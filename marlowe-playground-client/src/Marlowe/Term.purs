module Marlowe.Term where

import Prelude
import Control.Alt ((<|>))
import Data.Array (foldMap, foldl, mapWithIndex, (:))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.BigInteger (BigInteger)
import Data.Foldable (intercalate)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String (length)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Marlowe.Pretty (class Pretty, genericPretty, prettyFragment)
import Marlowe.Semantics (ChosenNum, Money, Party, PubKey, Slot, Timeout)
import Marlowe.Semantics as S
import Text.Parsing.Parser.Basic (replaceInPosition)
import Text.Parsing.Parser.Pos (Position(..))
import Text.PrettyPrint.Leijen (appendWithSoftbreak)
import Text.PrettyPrint.Leijen as Leijen
import Type.Proxy (Proxy(Proxy))

data MarloweType
  = StringType
  | BigIntegerType
  | SlotType
  | AccountIdType
  | ChoiceIdType
  | ValueIdType
  | ActionType
  | PayeeType
  | CaseType
  | ValueType
  | InputType
  | ObservationType
  | ContractType
  | BoundType

derive instance eqMarloweType :: Eq MarloweType

data Argument
  = ArrayArg String
  | DataArg String
  | NewtypeArg

getMarloweConstructors :: MarloweType -> Map String (Array Argument)
getMarloweConstructors StringType = Map.singleton "String" [ NewtypeArg ]

getMarloweConstructors BigIntegerType = Map.singleton "Integer" [ NewtypeArg ]

getMarloweConstructors SlotType = Map.singleton "Integer" [ NewtypeArg ]

getMarloweConstructors AccountIdType = Map.singleton "AccountId" [ DataArg "accNumber", DataArg "accHolder" ]

getMarloweConstructors ChoiceIdType = Map.singleton "ChoiceId" [ DataArg "choiceNumber", DataArg "choiceOwner" ]

getMarloweConstructors ValueIdType = Map.singleton "ValueId" [ NewtypeArg ]

getMarloweConstructors ActionType =
  Map.fromFoldable
    [ (Tuple "Deposit" [ DataArg "accountId", DataArg "party", DataArg "value" ])
    , (Tuple "Choice" [ DataArg "choiceId", ArrayArg "bounds" ])
    , (Tuple "Notify" [ DataArg "observation" ])
    ]

getMarloweConstructors PayeeType =
  Map.fromFoldable
    [ (Tuple "Account" [ DataArg "accountId" ])
    , (Tuple "Party" [ DataArg "party" ])
    ]

getMarloweConstructors CaseType = Map.singleton "Case" [ DataArg "action", DataArg "contract" ]

getMarloweConstructors ValueType =
  Map.fromFoldable
    [ (Tuple "AvailableMoney" [ DataArg "accountId" ])
    , (Tuple "Constant" [ DataArg "amount" ])
    , (Tuple "NegValue" [ DataArg "value" ])
    , (Tuple "AddValue" [ DataArg "value", DataArg "value" ])
    , (Tuple "SubValue" [ DataArg "value", DataArg "value" ])
    , (Tuple "ChoiceValue" [ DataArg "choiceId", DataArg "value" ])
    , (Tuple "SlotIntervalStart" [])
    , (Tuple "SlotIntervalEnd" [])
    , (Tuple "UseValue" [ DataArg "valueId" ])
    ]

getMarloweConstructors InputType =
  Map.fromFoldable
    [ (Tuple "IDeposit" [ DataArg "accountId", DataArg "party", DataArg "money" ])
    , (Tuple "IChoice" [ DataArg "choiceId", DataArg "choiceNum" ])
    , (Tuple "INotify" [])
    ]

getMarloweConstructors ObservationType =
  Map.fromFoldable
    [ (Tuple "AndObs" [ DataArg "observation", DataArg "observation" ])
    , (Tuple "OrObs" [ DataArg "observation", DataArg "observation" ])
    , (Tuple "NotObs" [ DataArg "observation" ])
    , (Tuple "ChoseSomething" [ DataArg "choiceId" ])
    , (Tuple "ValueGE" [ DataArg "value", DataArg "value" ])
    , (Tuple "ValueGT" [ DataArg "value", DataArg "value" ])
    , (Tuple "ValueLE" [ DataArg "value", DataArg "value" ])
    , (Tuple "ValueLT" [ DataArg "value", DataArg "value" ])
    , (Tuple "ValueEQ" [ DataArg "value", DataArg "value" ])
    , (Tuple "TrueObs" [])
    , (Tuple "FalseObs" [])
    ]

getMarloweConstructors ContractType =
  Map.fromFoldable
    [ (Tuple "Close" [])
    , (Tuple "Pay" [ DataArg "accountId", DataArg "payee", DataArg "value", DataArg "contract" ])
    , (Tuple "If" [ DataArg "observation", DataArg "contract", DataArg "contract" ])
    , (Tuple "When" [ ArrayArg "case", DataArg "timeout", DataArg "contract" ])
    , (Tuple "Let" [ DataArg "valueId", DataArg "value", DataArg "contract" ])
    ]

getMarloweConstructors BoundType = Map.singleton "Bound" [ DataArg "from", DataArg "to" ]

constructMarloweType :: String -> MarloweHole -> Map String (Array Argument) -> String
constructMarloweType constructorName (MarloweHole { name, marloweType, start }) m = case Map.lookup constructorName m of
  Nothing -> ""
  Just [] -> constructorName
  Just vs ->
    let
      Position { line, column } = start
    in
      "(" <> constructorName <> " " <> intercalate " " (mapWithIndex (showArgument line column) vs) <> ")"
  where
  showArgument line column i (ArrayArg arg) = "[ ?" <> arg <> "_" <> show line <> "_" <> show (column + i) <> " ]"

  showArgument line column i (DataArg arg) = "?" <> arg <> "_" <> show line <> "_" <> show (column + i)

  showArgument _ _ _ NewtypeArg = ""

derive instance genericMarloweType :: Generic MarloweType _

instance showMarloweType :: Show MarloweType where
  show = genericShow

zeroPosition :: Position
zeroPosition = Position { line: 0, column: 0 }

data Term a
  = Term a Position Position
  | Hole String (Proxy a) Position Position
  | Const String (Proxy a) Position Position

derive instance genericTerm :: Generic (Term a) _

instance eqTerm :: Eq a => Eq (Term a) where
  eq a b = genericEq a b

instance showTerm :: Show a => Show (Term a) where
  show (Term a _ _) = show a
  show (Hole name _ _ _) = "?" <> name
  show (Const name _ _ _) = name

instance prettyTerm :: Pretty a => Pretty (Term a) where
  prettyFragment (Term a _ _) = prettyFragment a
  prettyFragment (Hole name _ _ _) = Leijen.text $ "?" <> name
  prettyFragment (Const name _ _ _) = Leijen.text name

-- a concrete type for holes only
data MarloweHole
  = MarloweHole
    { name :: String
    , marloweType :: MarloweType
    , start :: Position
    , end :: Position
    }

derive instance eqMarloweHole :: Eq MarloweHole

instance ordMarloweHole :: Ord MarloweHole where
  compare (MarloweHole { start: (Position a) }) (MarloweHole { start: (Position b) }) = (compare `on` _.line <> compare `on` _.column) a b

-- a concrete type for constants only
data MarloweConstant
  = MarloweConstant
    { name :: String
    , marloweType :: MarloweType
    , start :: Position
    , end :: Position
    }

derive instance eqMarloweConstant :: Eq MarloweConstant

instance ordMarloweConstant :: Ord MarloweConstant where
  compare (MarloweConstant { start: (Position a) }) (MarloweConstant { start: (Position b) }) = (compare `on` _.line <> compare `on` _.column) a b

data Refactoring
  = ExtractAccountId { name :: String, accountId :: S.AccountId, start :: Position, end :: Position }
  | RenameConstant { oldName :: String, newName :: String }
  | InjectAccountIds (Map String S.AccountId)

class IsMarloweType a where
  marloweType :: Proxy a -> MarloweType

instance stringIsMarloweType :: IsMarloweType String where
  marloweType _ = StringType

instance bigIntegerIsMarloweType :: IsMarloweType BigInteger where
  marloweType _ = BigIntegerType

-- a Monoid for collecting Holes
newtype Holes
  = Holes (Map String (Array MarloweHole))

derive instance newtypeHoles :: Newtype Holes _

instance semigroupHoles :: Semigroup Holes where
  append (Holes a) (Holes b) = Holes (Map.unionWith append a b)

instance monoidHoles :: Monoid Holes where
  mempty = Holes mempty

-- a Monoid for collecting Constants
newtype Constants
  = Constants (Map String (NonEmptyArray MarloweConstant))

derive instance newtypeConstants :: Newtype Constants _

instance semigroupConstants :: Semigroup Constants where
  append (Constants a) (Constants b) = Constants (Map.unionWith append a b)

instance monoidConstants :: Monoid Constants where
  mempty = Constants mempty

insertHole :: forall a. IsMarloweType a => Holes -> Term a -> Holes
insertHole (Holes m) (Hole name proxy start end) = Holes $ Map.alter f name m
  where
  marloweHole = MarloweHole { name, marloweType: (marloweType proxy), start, end }

  f v = Just (marloweHole : fromMaybe [] v)

insertHole m _ = m

insertConstant :: forall a. IsMarloweType a => Constants -> Term a -> Constants
insertConstant (Constants m) (Const name proxy start end) = Constants $ Map.alter f name m
  where
  marloweConstant = MarloweConstant { name, marloweType: (marloweType proxy), start, end }

  f :: Maybe (NonEmptyArray MarloweConstant) -> Maybe (NonEmptyArray MarloweConstant)
  f v = case v of
    Just cs -> Just $ NEA.cons marloweConstant cs
    Nothing -> Just $ NEA.singleton marloweConstant

insertConstant m _ = m

class HasMarloweHoles a where
  getMetadata :: Metadata -> a -> Metadata
  doRefactoring :: Refactoring -> a -> a

newtype Metadata
  = Metadata
  { holes :: Holes
  , constants :: Constants
  , position :: Position
  , refactoring :: Maybe Refactoring
  }

derive instance newtypeMetadata :: Newtype Metadata _

instance semigroupMetadata :: Semigroup Metadata where
  append (Metadata a) (Metadata b) =
    Metadata
      { holes: a.holes <> b.holes
      , constants: a.constants <> b.constants
      , position: a.position
      , refactoring: a.refactoring <|> b.refactoring
      }

instance monoidMetadata :: Monoid Metadata where
  mempty =
    Metadata
      { holes: mempty
      , constants: mempty
      , position: Position { column: zero, line: zero }
      , refactoring: Nothing
      }

_constants :: Lens' Metadata Constants
_constants = _Newtype <<< prop (SProxy :: SProxy "constants")

mkMetadata :: Position -> Metadata
mkMetadata position =
  Metadata
    { holes: mempty
    , constants: mempty
    , position: position
    , refactoring: Nothing
    }

positionWithin :: Position -> Position -> Position -> Boolean
positionWithin (Position needle) (Position start) (Position finish) =
  needle.line >= start.line
    && needle.line
    <= finish.line
    && needle.column
    >= start.column
    && needle.column
    <= finish.column

getExtractAccountId :: Position -> AccountId -> Position -> Position -> Maybe Refactoring
getExtractAccountId cursor accountIdTerm start@(Position s) end =
  if positionWithin cursor start end then case fromTerm accountIdTerm of
    Nothing -> Nothing
    Just accountId@(S.AccountId accNumber accName) ->
      let
        name = "account_" <> show accNumber <> "_" <> accName
      in
        Just $ ExtractAccountId { name, accountId, start, end }
  else
    Nothing

instance termHasMarloweHoles :: (IsMarloweType a, HasMarloweHoles a) => HasMarloweHoles (Term a) where
  getMetadata m (Term a _ _) = getMetadata m a
  getMetadata (Metadata m) c@(Const _ _ _ _) = Metadata $ m { constants = insertConstant m.constants c }
  getMetadata (Metadata m) h@(Hole _ _ _ _) = Metadata $ m { holes = insertHole m.holes h }
  doRefactoring r (Term a b c) = Term (doRefactoring r a) b c
  doRefactoring (RenameConstant r) c@(Const name proxy start end) = if name == r.oldName then Const r.newName proxy start end else c
  doRefactoring _ a = a

instance arrayHasMarloweHoles :: HasMarloweHoles a => HasMarloweHoles (Array a) where
  getMetadata m as =
    let
      inner = unwrap $ foldMap (getMetadata m) as
    in
      Metadata
        (unwrap m)
          { holes = inner.holes
          , constants = inner.constants
          , refactoring = inner.refactoring
          }
  doRefactoring r as = map (doRefactoring r) as

-- Parsable versions of the Marlowe types
data Bound
  = Bound (Term BigInteger) (Term BigInteger)

derive instance genericBound :: Generic Bound _

instance showBound :: Show Bound where
  show v = genericShow v

instance prettyBound :: Pretty Bound where
  prettyFragment a = Leijen.text $ show a

instance boundFromTerm :: FromTerm Bound S.Bound where
  fromTerm (Bound a b) = S.Bound <$> termToValue a <*> termToValue b

instance boundIsMarloweType :: IsMarloweType Bound where
  marloweType _ = BoundType

instance boundHasMarloweHoles :: HasMarloweHoles Bound where
  getMetadata (Metadata m) (Bound a b) =
    Metadata
      m
        { holes = insertHole m.holes a <> insertHole m.holes b
        , constants = insertConstant m.constants a <> insertConstant m.constants b
        }
  doRefactoring _ b = b

data AccountId
  = AccountId (Term BigInteger) (Term PubKey)

derive instance genericAccountId :: Generic AccountId _

instance showAccountId :: Show AccountId where
  show v = genericShow v

instance prettyAccountId :: Pretty AccountId where
  prettyFragment a = Leijen.text (show a)

instance accountIdFromTerm :: FromTerm AccountId S.AccountId where
  fromTerm (AccountId (Term b _ _) (Term c _ _)) = pure $ S.AccountId b c
  fromTerm _ = Nothing

instance accountIdToTerm :: ToTerm S.AccountId AccountId where
  toTerm (S.AccountId accNumber accName) = Term (AccountId (toTerm accNumber) (toTerm accName)) zeroPosition zeroPosition

instance accountIdIsMarloweType :: IsMarloweType AccountId where
  marloweType _ = AccountIdType

instance accountIdHasMarloweHoles :: HasMarloweHoles AccountId where
  getMetadata (Metadata m) (AccountId a b) =
    Metadata
      m
        { holes = insertHole m.holes a <> insertHole m.holes b
        , constants = insertConstant m.constants a <> insertConstant m.constants b
        }
  doRefactoring _ a = a

data ChoiceId
  = ChoiceId (Term String) (Term PubKey)

derive instance genericChoiceId :: Generic ChoiceId _

instance showChoiceId :: Show ChoiceId where
  show v = genericShow v

instance prettyChoiceId :: Pretty ChoiceId where
  prettyFragment a = Leijen.text (show a)

instance choiceIdFromTerm :: FromTerm ChoiceId S.ChoiceId where
  fromTerm (ChoiceId (Term a _ _) (Term b _ _)) = pure $ S.ChoiceId a b
  fromTerm _ = Nothing

instance choiceIdIsMarloweType :: IsMarloweType ChoiceId where
  marloweType _ = ChoiceIdType

instance choiceIdHasMarloweHoles :: HasMarloweHoles ChoiceId where
  getMetadata (Metadata m) (ChoiceId a b) =
    Metadata
      m
        { holes = insertHole m.holes a <> insertHole m.holes b
        , constants = insertConstant m.constants a <> insertConstant m.constants b
        }
  doRefactoring _ c = c

data Action
  = Deposit (Term AccountId) (Term Party) (Term Value)
  | Choice (Term ChoiceId) (Array (Term Bound))
  | Notify (Term Observation)

derive instance genericAction :: Generic Action _

instance showAction :: Show Action where
  show v = genericShow v

instance prettyAction :: Pretty Action where
  prettyFragment a = Leijen.text (show a)

instance actionFromTerm :: FromTerm Action S.Action where
  fromTerm (Deposit a b c) = S.Deposit <$> fromTerm a <*> termToValue b <*> fromTerm c
  fromTerm (Choice a b) = S.Choice <$> fromTerm a <*> (traverse fromTerm b)
  fromTerm (Notify a) = S.Notify <$> fromTerm a

instance actionMarloweType :: IsMarloweType Action where
  marloweType _ = ActionType

instance actionHasMarloweHoles :: HasMarloweHoles Action where
  getMetadata m (Deposit a b c) =
    let
      left = unwrap $ getMetadata m a

      right = unwrap $ getMetadata m c

      holes = insertHole (unwrap m).holes b

      constants = insertConstant (unwrap m).constants b

      refactoring = case a of
        (Term accountId start end) -> getExtractAccountId (unwrap m).position accountId start end <|> right.refactoring
        _ -> left.refactoring <|> right.refactoring
    in
      Metadata
        (unwrap m)
          { holes = left.holes <> holes <> right.holes
          , constants = left.constants <> constants <> right.constants
          , refactoring = refactoring
          }
  getMetadata m (Choice a bs) = getMetadata m a <> getMetadata m bs
  getMetadata m (Notify a) = getMetadata m a
  doRefactoring (ExtractAccountId r) deposit@(Deposit (Term (AccountId (Term a _ _) (Term b _ _)) start end) c d) =
    let
      (S.AccountId a1 b1) = r.accountId

      refactoredD = doRefactoring (ExtractAccountId r) d
    in
      if a == a1 && b == b1 then
        Deposit (Const r.name Proxy start end) c refactoredD
      else
        deposit
  doRefactoring (InjectAccountIds m) deposit@(Deposit (Const name proxy start end) c d) = case Map.lookup name m of
    Just acc -> Deposit (toTerm acc) c d
    _ -> deposit
  doRefactoring r (Notify o) = Notify $ doRefactoring r o
  doRefactoring _ a = a

data Payee
  = Account (Term AccountId)
  | Party (Term Party)

derive instance genericPayee :: Generic Payee _

instance showPayee :: Show Payee where
  show v = genericShow v

instance prettyPayee :: Pretty Payee where
  prettyFragment a = genericPretty a

instance payeeFromTerm :: FromTerm Payee S.Payee where
  fromTerm (Account a) = S.Account <$> fromTerm a
  fromTerm (Party (Term a _ _)) = pure $ S.Party a
  fromTerm _ = Nothing

instance payeeMarloweType :: IsMarloweType Payee where
  marloweType _ = PayeeType

instance payeeHasMarloweHoles :: HasMarloweHoles Payee where
  getMetadata m (Account a) =
    let
      inner = unwrap $ getMetadata m a

      refactoring = case a of
        (Term accountId start end) -> getExtractAccountId (unwrap m).position accountId start end
        _ -> Nothing
    in
      Metadata
        (unwrap m)
          { holes = inner.holes
          , constants = inner.constants
          , refactoring = refactoring
          }
  getMetadata (Metadata m) (Party a) = Metadata m { holes = insertHole m.holes a, constants = insertConstant m.constants a }
  doRefactoring (ExtractAccountId r) account@(Account (Term (AccountId (Term a _ _) (Term b _ _)) start end)) =
    let
      (S.AccountId a1 b1) = r.accountId
    in
      if a == a1 && b == b1 then
        Account (Const r.name Proxy start end)
      else
        account
  doRefactoring (InjectAccountIds m) account@(Account (Const name proxy start end)) = case Map.lookup name m of
    Just acc -> Account (toTerm acc)
    _ -> account
  doRefactoring _ a = a

data Case
  = Case (Term Action) (Term Contract)

derive instance genericCase :: Generic Case _

instance showCase :: Show Case where
  show v = genericShow v

-- FIXME: pretty printing is a disaster and slooooowwwww
instance prettyCase :: Pretty Case where
  prettyFragment (Case action' contract') = appendWithSoftbreak (Leijen.text "Case " <> prettyFragment action' <> Leijen.text " ") (prettyFragment contract')

instance caseFromTerm :: FromTerm Case S.Case where
  fromTerm (Case a b) = S.Case <$> fromTerm a <*> fromTerm b

instance caseMarloweType :: IsMarloweType Case where
  marloweType _ = CaseType

instance caseHasMarloweHoles :: HasMarloweHoles Case where
  getMetadata m (Case a b) = getMetadata m a <> getMetadata m b
  doRefactoring r (Case a b) = Case (doRefactoring r a) (doRefactoring r b)

data Value
  = AvailableMoney (Term AccountId)
  | Constant (Term BigInteger)
  | NegValue (Term Value)
  | AddValue (Term Value) (Term Value)
  | SubValue (Term Value) (Term Value)
  | ChoiceValue (Term ChoiceId) (Term Value)
  | SlotIntervalStart
  | SlotIntervalEnd
  | UseValue (Term ValueId)

derive instance genericValue :: Generic Value _

instance showValue :: Show Value where
  show v = genericShow v

instance prettyValue :: Pretty Value where
  prettyFragment a = genericPretty a

instance valueFromTerm :: FromTerm Value S.Value where
  fromTerm (AvailableMoney a) = S.AvailableMoney <$> fromTerm a
  fromTerm (Constant a) = S.Constant <$> termToValue a
  fromTerm (NegValue a) = S.NegValue <$> fromTerm a
  fromTerm (AddValue a b) = S.AddValue <$> fromTerm a <*> fromTerm b
  fromTerm (SubValue a b) = S.SubValue <$> fromTerm a <*> fromTerm b
  fromTerm (ChoiceValue a b) = S.ChoiceValue <$> fromTerm a <*> fromTerm b
  fromTerm SlotIntervalStart = pure S.SlotIntervalStart
  fromTerm SlotIntervalEnd = pure S.SlotIntervalEnd
  fromTerm (UseValue a) = S.UseValue <$> fromTerm a

instance valueIsMarloweType :: IsMarloweType Value where
  marloweType _ = ValueType

instance valueHasMarloweHoles :: HasMarloweHoles Value where
  getMetadata m (AvailableMoney a) =
    let
      inner = unwrap $ getMetadata m a

      refactoring = case a of
        (Term accountId start end) -> getExtractAccountId (unwrap m).position accountId start end
        _ -> Nothing
    in
      Metadata
        (unwrap m)
          { holes = inner.holes
          , constants = inner.constants
          , refactoring = refactoring
          }
  getMetadata (Metadata m) (Constant a) =
    Metadata
      m
        { holes = insertHole m.holes a
        , constants = insertConstant m.constants a
        }
  getMetadata m (NegValue a) = getMetadata m a
  getMetadata m (AddValue a b) = getMetadata m a <> getMetadata m b
  getMetadata m (SubValue a b) = getMetadata m a <> getMetadata m b
  getMetadata m (ChoiceValue a b) = getMetadata m a <> getMetadata m b
  getMetadata m SlotIntervalStart = mempty
  getMetadata m SlotIntervalEnd = mempty
  getMetadata m (UseValue a) = getMetadata m a
  doRefactoring (ExtractAccountId r) availableMoney@(AvailableMoney (Term (AccountId (Term a _ _) (Term b _ _)) start end)) =
    let
      (S.AccountId a1 b1) = r.accountId
    in
      if a == a1 && b == b1 then
        AvailableMoney (Const r.name Proxy start end)
      else
        availableMoney
  doRefactoring (InjectAccountIds m) availableMoney@(AvailableMoney (Const name proxy start end)) = case Map.lookup name m of
    Just acc -> AvailableMoney (toTerm acc)
    _ -> availableMoney
  doRefactoring r (NegValue a) = NegValue $ doRefactoring r a
  doRefactoring r (AddValue a b) = AddValue (doRefactoring r a) (doRefactoring r b)
  doRefactoring r (SubValue a b) = SubValue (doRefactoring r a) (doRefactoring r b)
  doRefactoring r (ChoiceValue a b) = ChoiceValue a $ doRefactoring r b
  doRefactoring _ a = a

data Input
  = IDeposit (Term AccountId) (Term Party) (Term Money)
  | IChoice (Term ChoiceId) (Term ChosenNum)
  | INotify

data Observation
  = AndObs (Term Observation) (Term Observation)
  | OrObs (Term Observation) (Term Observation)
  | NotObs (Term Observation)
  | ChoseSomething (Term ChoiceId)
  | ValueGE (Term Value) (Term Value)
  | ValueGT (Term Value) (Term Value)
  | ValueLT (Term Value) (Term Value)
  | ValueLE (Term Value) (Term Value)
  | ValueEQ (Term Value) (Term Value)
  | TrueObs
  | FalseObs

derive instance genericObservation :: Generic Observation _

instance showObservation :: Show Observation where
  show v = genericShow v

instance prettyObservation :: Pretty Observation where
  prettyFragment a = genericPretty a

instance fromTermTerm :: FromTerm a b => FromTerm (Term a) b where
  fromTerm (Term a _ _) = fromTerm a
  fromTerm _ = Nothing

instance observationFromTerm :: FromTerm Observation S.Observation where
  fromTerm (AndObs a b) = S.AndObs <$> fromTerm a <*> fromTerm b
  fromTerm (OrObs a b) = S.OrObs <$> fromTerm a <*> fromTerm b
  fromTerm (NotObs a) = S.NotObs <$> fromTerm a
  fromTerm (ChoseSomething a) = S.ChoseSomething <$> fromTerm a
  fromTerm (ValueGE a b) = S.ValueGE <$> fromTerm a <*> fromTerm b
  fromTerm (ValueGT a b) = S.ValueGT <$> fromTerm a <*> fromTerm b
  fromTerm (ValueLT a b) = S.ValueLT <$> fromTerm a <*> fromTerm b
  fromTerm (ValueLE a b) = S.ValueLE <$> fromTerm a <*> fromTerm b
  fromTerm (ValueEQ a b) = S.ValueEQ <$> fromTerm a <*> fromTerm b
  fromTerm TrueObs = pure S.TrueObs
  fromTerm FalseObs = pure S.FalseObs

instance observationIsMarloweType :: IsMarloweType Observation where
  marloweType _ = ObservationType

instance observationHasMarloweHoles :: HasMarloweHoles Observation where
  getMetadata m (AndObs a b) = getMetadata m a <> getMetadata m b
  getMetadata m (OrObs a b) = getMetadata m a <> getMetadata m b
  getMetadata m (NotObs a) = getMetadata m a
  getMetadata m (ChoseSomething a) = getMetadata m a
  getMetadata m (ValueGE a b) = getMetadata m a <> getMetadata m b
  getMetadata m (ValueGT a b) = getMetadata m a <> getMetadata m b
  getMetadata m (ValueLT a b) = getMetadata m a <> getMetadata m b
  getMetadata m (ValueLE a b) = getMetadata m a <> getMetadata m b
  getMetadata m (ValueEQ a b) = getMetadata m a <> getMetadata m b
  getMetadata m TrueObs = mempty
  getMetadata m FalseObs = mempty
  doRefactoring r (AndObs a b) = AndObs (doRefactoring r a) (doRefactoring r b)
  doRefactoring r (OrObs a b) = OrObs (doRefactoring r a) (doRefactoring r b)
  doRefactoring r (NotObs a) = NotObs $ doRefactoring r a
  doRefactoring r (ValueGE a b) = ValueGE (doRefactoring r a) (doRefactoring r b)
  doRefactoring r (ValueGT a b) = ValueGT (doRefactoring r a) (doRefactoring r b)
  doRefactoring r (ValueLT a b) = ValueLT (doRefactoring r a) (doRefactoring r b)
  doRefactoring r (ValueLE a b) = ValueLE (doRefactoring r a) (doRefactoring r b)
  doRefactoring r (ValueEQ a b) = ValueEQ (doRefactoring r a) (doRefactoring r b)
  doRefactoring r a = a

data Contract
  = Close
  | Pay (Term AccountId) (Term Payee) (Term Value) (Term Contract)
  | If (Term Observation) (Term Contract) (Term Contract)
  | When (Array (Term Case)) (Term Timeout) (Term Contract)
  | Let (Term ValueId) (Term Value) (Term Contract)

derive instance genericContract :: Generic Contract _

instance showContract :: Show Contract where
  show v = genericShow v

instance prettyContract :: Pretty Contract where
  prettyFragment a = genericPretty a

instance contractFromTerm :: FromTerm Contract S.Contract where
  fromTerm Close = pure S.Close
  fromTerm (Pay a b c d) = S.Pay <$> fromTerm a <*> fromTerm b <*> fromTerm c <*> fromTerm d
  fromTerm (If a b c) = S.If <$> fromTerm a <*> fromTerm b <*> fromTerm c
  fromTerm (When as b c) = S.When <$> (traverse fromTerm as) <*> termToValue b <*> fromTerm c
  fromTerm (Let a b c) = S.Let <$> fromTerm a <*> fromTerm b <*> fromTerm c

instance contractIsMarloweType :: IsMarloweType Contract where
  marloweType _ = ContractType

instance contractHasMarloweHoles :: HasMarloweHoles Contract where
  getMetadata m Close = mempty
  getMetadata m (Pay a b c d) =
    let
      newM = getMetadata m a <> getMetadata m b <> getMetadata m c <> getMetadata m d

      refactoring = case a of
        (Term accountId start end) ->
          getExtractAccountId (unwrap m).position accountId start end
            <|> (unwrap newM).refactoring
        _ -> (unwrap newM).refactoring
    in
      Metadata
        (unwrap m)
          { holes = (unwrap newM).holes
          , constants = (unwrap newM).constants
          , refactoring = refactoring
          }
  getMetadata m (If a b c) = getMetadata m a <> getMetadata m b <> getMetadata m c
  getMetadata m (When as b c) =
    let
      left = unwrap $ getMetadata m as

      right = unwrap $ getMetadata m c

      holes = insertHole (unwrap m).holes b

      constants = insertConstant (unwrap m).constants b

      refactoring = left.refactoring <|> right.refactoring
    in
      Metadata
        (unwrap m)
          { holes = left.holes <> holes <> right.holes
          , constants = left.constants <> constants <> right.constants
          , refactoring = refactoring
          }
  getMetadata m (Let a b c) = getMetadata m a <> getMetadata m b <> getMetadata m c
  doRefactoring r Close = Close
  doRefactoring (ExtractAccountId r) pay@(Pay (Term (AccountId (Term a _ _) (Term b _ _)) start end) c d e) =
    let
      (S.AccountId a1 b1) = r.accountId

      refactoredD = doRefactoring (ExtractAccountId r) d

      refactoredE = doRefactoring (ExtractAccountId r) e
    in
      if a == a1 && b == b1 then
        Pay (Const r.name Proxy start end) c refactoredD refactoredE
      else
        pay
  doRefactoring (InjectAccountIds m) pay@(Pay (Const name proxy start end) c d e) = case Map.lookup name m of
    Just acc -> Pay (toTerm acc) c d e
    _ -> pay
  doRefactoring r (Pay a b c d) = Pay (doRefactoring r a) (doRefactoring r b) (doRefactoring r c) (doRefactoring r d)
  doRefactoring r (If a b c) = If (doRefactoring r a) (doRefactoring r b) (doRefactoring r c)
  doRefactoring r (When a b c) = When (doRefactoring r a) b (doRefactoring r c)
  doRefactoring r (Let a b c) = Let a (doRefactoring r b) (doRefactoring r c)

newtype ValueId
  = ValueId String

derive instance genericValueId :: Generic ValueId _

instance showValueId :: Show ValueId where
  show (ValueId valueId') = show valueId'

instance prettyValueId :: Pretty ValueId where
  prettyFragment a = Leijen.text (show a)

instance valueIdFromTerm :: FromTerm ValueId S.ValueId where
  fromTerm (ValueId a) = pure $ S.ValueId a

instance valueIdIsMarloweType :: IsMarloweType ValueId where
  marloweType _ = ValueIdType

instance valueIdHasMarloweHoles :: HasMarloweHoles ValueId where
  getMetadata m _ = m
  doRefactoring _ a = a

termToValue :: forall a. Term a -> Maybe a
termToValue (Term a _ _) = Just a

termToValue _ = Nothing

class FromTerm a b where
  fromTerm :: a -> Maybe b

class ToTerm a b where
  toTerm :: a -> Term b

instance bigIntegerToTerm :: ToTerm BigInteger BigInteger where
  toTerm n = Term n zeroPosition zeroPosition

instance stringToTerm :: ToTerm String String where
  toTerm n = Term n zeroPosition zeroPosition

instance slotMarloweType :: IsMarloweType Slot where
  marloweType _ = SlotType

-- Replace all holes of a certain name with the value
replaceInPositions :: String -> MarloweHole -> Array MarloweHole -> String -> String
replaceInPositions constructor firstHole@(MarloweHole { marloweType }) holes currentContract =
  let
    offset (Position { line, column }) x = Position { line, column: column + x }

    lengthOfReplacement value (Position { column: start }) (Position { column: end }) = (length value) - (end - start)

    getLine (Position { line }) = line

    m = getMarloweConstructors marloweType

    holeString = constructMarloweType constructor firstHole m

    (_ /\ _ /\ final) =
      ( foldl
          ( \(currLength /\ currLineNumber /\ currString) hole@(MarloweHole { name, marloweType, start, end }) ->
              let
                thisLine = getLine start

                thisLength = currLength + (lengthOfReplacement holeString start end)
              in
                if currLineNumber == thisLine then
                  ( thisLength
                      /\ thisLine
                      /\ (replaceInPosition (offset start thisLength) (offset end thisLength) holeString currString)
                  )
                else
                  ( 0
                      /\ thisLine
                      /\ (replaceInPosition start end holeString currString)
                  )
          )
          (0 /\ 0 /\ currentContract)
          holes
      )
  in
    final
