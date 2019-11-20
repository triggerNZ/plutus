module Simulation.StateTable where

import Bootstrap (btn, btnSmall, cardBody_, card_)
import Control.Alternative (map)
import Data.Array as Array
import Data.BigInteger (BigInteger)
import Data.Eq ((==))
import Data.Lens (to, view, (^.))
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Halogen.HTML (ClassName(..), HTML, col, colgroup, div, h3_, span, strong_, table_, tbody_, td, td_, text, th, th_, thead_, tr)
import Halogen.HTML.Properties (class_, classes)
import Marlowe.Semantics (AccountId(..), Ada, ChoiceId(..), ChosenNum, Payment(..), ValueId(..), _accounts, _boundValues, _choices)
import Prelude (show, ($), (<<<))
import Simulation.Common (paneHeader)
import Types (FrontendState, HAction, _Head, _marloweState, _moneyInContract, _payments, _slot, _state)

stateTitle ::
  forall p.
  FrontendState ->
  HTML p HAction
stateTitle state =
  div
    [ classes
        [ ClassName "demos"
        , ClassName "d-flex"
        , ClassName "flex-row"
        , ClassName "align-items-center"
        , ClassName "justify-content-between"
        , ClassName "mt-3"
        , ClassName "mb-3"
        ]
    ]
    [ paneHeader "State"
    , span
        [ classes
            [ btn
            , btnSmall
            ]
        ]
        [ strong_
            [ text "Current Block:"
            ]
        , span
            [ class_ $ ClassName "block-number"
            ]
            [ view (_marloweState <<< _Head <<< _slot <<< to show <<< to text) state
            ]
        , strong_
            [ text "Money in contract:"
            ]
        , span
            [ class_ $ ClassName "money-in-contract"
            ]
            [ view (_marloweState <<< _Head <<< _moneyInContract <<< to show <<< to text) state
            ]
        , strong_ [ text "ADA" ]
        ]
    ]

statePane :: forall p. FrontendState -> HTML p HAction
statePane state =
  div
    [ class_ $ ClassName "col"
    ]
    [ stateTable state
    ]

stateTable :: forall p. FrontendState -> HTML p HAction
stateTable state =
  div
    [ class_ $ ClassName "full-width-card"
    ]
    [ card_
        [ cardBody_
            [ h3_
                [ text "Accounts"
                ]
            , div
                [ class_ $ ClassName "state-row" ]
                [ if (Map.size accounts == 0) then
                    text "There are no accounts in the state"
                  else
                    renderAccounts accounts
                ]
            , h3_
                [ text "Choices"
                ]
            , div
                [ class_ $ ClassName "state-row" ]
                [ if (Map.size choices == 0) then
                    text "No choices have been recorded"
                  else
                    renderChoices choices
                ]
            , h3_
                [ text "Payments"
                ]
            , div
                [ class_ $ ClassName "state-row" ]
                [ if (Array.length payments == 0) then
                    text "No payments have been recorded"
                  else
                    renderPayments payments
                ]
            , h3_
                [ text "Let bindings"
                ]
            , div
                [ class_ $ ClassName "state-last-row" ]
                [ if (Map.size bindings == 0) then
                    text "No values have been bound"
                  else
                    renderBindings bindings
                ]
            ]
        ]
    ]
  where
  accounts = state ^. _marloweState <<< _Head <<< _state <<< _accounts

  choices = state ^. _marloweState <<< _Head <<< _state <<< _choices

  payments = state ^. _marloweState <<< _Head <<< _payments

  bindings = state ^. _marloweState <<< _Head <<< _state <<< _boundValues

renderAccounts :: forall p. Map AccountId Ada -> HTML p HAction
renderAccounts accounts =
  table_
    [ colgroup []
        [ col []
        , col []
        , col []
        ]
    , thead_
        [ tr []
            [ th_
                [ text "Account id"
                ]
            , th
                [ class_ $ ClassName "middle-column"
                ]
                [ text "Participant"
                ]
            , th_
                [ text "Money"
                ]
            ]
        ]
    , tbody_ (map renderAccount accountList)
    ]
  where
  accountList = Map.toUnfoldable accounts :: Array (Tuple AccountId Ada)

renderAccount :: forall p. Tuple AccountId Ada -> HTML p HAction
renderAccount (Tuple (AccountId accountNumber accountOwner) value) =
  tr []
    [ td_
        [ text (show accountNumber)
        ]
    , td
        [ class_ $ ClassName "middle-column"
        ]
        [ text (show accountOwner)
        ]
    , td_
        [ text (show value)
        ]
    ]

renderChoices :: forall p. Map ChoiceId ChosenNum -> HTML p HAction
renderChoices choices =
  table_
    [ colgroup []
        [ col []
        , col []
        , col []
        ]
    , thead_
        [ tr []
            [ th_
                [ text "Choice id"
                ]
            , th
                [ class_ $ ClassName "middle-column"
                ]
                [ text "Participant"
                ]
            , th_
                [ text "Chosen value"
                ]
            ]
        ]
    , tbody_ (map renderChoice choiceList)
    ]
  where
  choiceList = Map.toUnfoldable choices :: Array (Tuple ChoiceId ChosenNum)

renderChoice :: forall p. Tuple ChoiceId ChosenNum -> HTML p HAction
renderChoice (Tuple (ChoiceId choiceName choiceOwner) value) =
  tr []
    [ td_
        [ text (show choiceName)
        ]
    , td
        [ class_ $ ClassName "middle-column"
        ]
        [ text (show choiceOwner)
        ]
    , td_
        [ text (show value)
        ]
    ]

renderPayments :: forall p. Array Payment -> HTML p HAction
renderPayments payments =
  table_
    [ colgroup []
        [ col []
        , col []
        , col []
        ]
    , thead_
        [ tr []
            [ th_
                [ text "Party"
                ]
            , th
                [ class_ $ ClassName "left-border-column"
                ]
                [ text "Money"
                ]
            ]
        ]
    , tbody_ (map renderPayment payments)
    ]

renderPayment :: forall p. Payment -> HTML p HAction
renderPayment (Payment party money) =
  tr []
    [ td_
        [ text party
        ]
    , td
        [ class_ $ ClassName "left-border-column"
        ]
        [ text (show money)
        ]
    ]

renderBindings :: forall p. Map ValueId BigInteger -> HTML p HAction
renderBindings bindings =
  table_
    [ colgroup []
        [ col []
        , col []
        , col []
        ]
    , thead_
        [ tr []
            [ th_
                [ text "Identifier"
                ]
            , th
                [ class_ $ ClassName "left-border-column"
                ]
                [ text "Value"
                ]
            ]
        ]
    , tbody_ (map renderBinding bindingList)
    ]
  where
  bindingList = Map.toUnfoldable bindings :: Array (Tuple ValueId BigInteger)

renderBinding :: forall p. Tuple ValueId BigInteger -> HTML p HAction
renderBinding (Tuple (ValueId valueId) value) =
  tr []
    [ td_
        [ text (show valueId)
        ]
    , td
        [ class_ $ ClassName "left-border-column"
        ]
        [ text (show value)
        ]
    ]
