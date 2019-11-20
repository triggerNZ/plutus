module Simulation.InputComposer where

import Bootstrap (cardBody_, card_, col6)
import Control.Alternative (map)
import Data.Array (catMaybes)
import Data.BigInteger (BigInteger, fromString, fromInt)
import Data.Eq ((==))
import Data.Foldable (foldMap, intercalate)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (view)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..), snd)
import Halogen.HTML (ClassName(..), HTML, b_, button, div, h3_, input, text)
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties (InputType(..), class_, classes, enabled, placeholder, type_, value)
import Marlowe.Semantics (AccountId(..), Bound(..), ChoiceId(..), ChosenNum, Input(..), Party, PubKey, inBounds)
import Prelude (class Show, const, show, ($), (<<<), (<>))
import Simulation.Common (isContractValid, paneHeader, spanText)
import Types (ActionInput(..), ActionInputId, FrontendState, HAction(..), _Head, _marloweState, _possibleActions)

inputComposerPane :: forall p. FrontendState -> HTML p HAction
inputComposerPane state =
  div
    [ classes
        [ col6
        , ClassName "input-composer"
        ]
    ]
    [ paneHeader "Input Composer"
    , div
        [ class_ $ ClassName "wallet"
        ]
        [ card_
            [ cardBody_ (inputComposer isEnabled (view (_marloweState <<< _Head <<< _possibleActions) state))
            ]
        ]
    ]
  where
  isEnabled = isContractValid state

inputComposer :: forall p. Boolean -> Map (Maybe PubKey) (Map ActionInputId ActionInput) -> Array (HTML p HAction)
inputComposer isEnabled actionInputs =
  if (Map.isEmpty actionInputs) then
    [ text "No valid inputs can be added to the transaction" ]
  else
    (actionsForPeople actionInputs)
  where
  kvs :: forall k v. Map k v -> Array (Tuple k v)
  kvs = Map.toUnfoldable

  vs :: forall k v. Map k v -> Array v
  vs m = map snd (kvs m)

  lastKey :: Maybe (Maybe PubKey)
  lastKey = map (\x -> x.key) (Map.findMax actionInputs)

  actionsForPeople :: forall q. Map (Maybe PubKey) (Map ActionInputId ActionInput) -> Array (HTML q HAction)
  actionsForPeople m = foldMap (\(Tuple k v) -> inputComposerPerson isEnabled k (vs v) (Just k == lastKey)) (kvs m)

inputComposerPerson ::
  forall p.
  Boolean ->
  Maybe PubKey ->
  Array ActionInput ->
  Boolean ->
  Array (HTML p HAction)
inputComposerPerson isEnabled maybePerson actionInputs isLast =
  [ h3_
      [ text
          ( case maybePerson of
              Just person -> ("Participant " <> show person)
              Nothing -> ("Anyone")
          )
      ]
  ]
    <> [ div [ class_ $ ClassName (if isLast then "state-last-row" else "state-row") ]
          (catMaybes (mapWithIndex inputForAction actionInputs))
      ]
  where
  inputForAction :: Int -> ActionInput -> Maybe (HTML p HAction)
  inputForAction index (DepositInput accountId party value) = Just $ inputDeposit isEnabled maybePerson index accountId party value

  inputForAction index (ChoiceInput choiceId bounds chosenNum) = Just $ inputChoice isEnabled maybePerson index choiceId chosenNum bounds

  inputForAction index NotifyInput = Just $ inputNotify isEnabled maybePerson index

inputDeposit ::
  forall p.
  Boolean ->
  Maybe PubKey ->
  Int ->
  AccountId ->
  Party ->
  BigInteger ->
  HTML p HAction
inputDeposit isEnabled person index accountId party value =
  let
    money = wrap value
  in
    flexRow_
      $ [ button
            [ class_ $ ClassName "composer-add-button"
            , enabled isEnabled
            , onClick $ const $ Just
                $ AddInput person (IDeposit accountId party money) []
            ]
            [ text "+"
            ]
        ]
      <> (renderDeposit accountId party value)

renderDeposit :: forall p. AccountId -> Party -> BigInteger -> Array (HTML p HAction)
renderDeposit (AccountId accountNumber accountOwner) party money =
  [ spanText "Deposit "
  , b_ [ spanText (show money) ]
  , spanText " ADA into Account "
  , b_ [ spanText (show accountOwner <> " (" <> show accountNumber <> ")") ]
  , spanText " as "
  , b_ [ spanText party ]
  ]

inputChoice :: forall p. Boolean -> Maybe PubKey -> Int -> ChoiceId -> ChosenNum -> Array Bound -> HTML p HAction
inputChoice isEnabled person index choiceId@(ChoiceId choiceName choiceOwner) chosenNum bounds =
  let
    validBounds = inBounds chosenNum bounds

    errorRow = if validBounds then [] else [ text boundsError ]
  in
    flexRow_
      ( [ button
            [ class_ $ ClassName "composer-add-button"
            , enabled isEnabled
            , onClick $ const $ Just
                $ AddInput person (IChoice (ChoiceId choiceName choiceOwner) chosenNum) bounds
            ]
            [ text "+"
            ]
        , spanText "Choice "
        , b_ [ spanText (show choiceName) ]
        , spanText ": Choose value "
        , marloweActionInput isEnabled (SetChoice choiceId) chosenNum
        ]
          <> errorRow
      )
  where
  boundsError = "Choice must be between " <> intercalate " or " (map boundError bounds)

  boundError (Bound from to) = show from <> " and " <> show to

inputNotify ::
  forall p.
  Boolean ->
  Maybe PubKey ->
  Int ->
  HTML p HAction
inputNotify isEnabled person index =
  flexRow_
    [ button
        [ class_ $ ClassName "composer-add-button"
        , enabled isEnabled
        , onClick $ const $ Just
            $ AddInput person INotify []
        ]
        [ text "+"
        ]
    , text $ "Notify contract"
    ]

marloweActionInput :: forall p a. Show a => Boolean -> (BigInteger -> HAction) -> a -> HTML p HAction
marloweActionInput isEnabled f current =
  input
    [ type_ InputNumber
    , enabled isEnabled
    , placeholder "BigInteger"
    , class_ $ ClassName "action-input"
    , value $ show current
    , onValueChange
        $ ( \x ->
              Just
                $ f
                    ( case fromString x of
                        Just y -> y
                        Nothing -> fromInt 0
                    )
          )
    ]

flexRow_ ::
  forall p.
  Array (HTML p HAction) ->
  HTML p HAction
flexRow_ html = div [ classes [ ClassName "d-flex", ClassName "flex-row" ] ] html
