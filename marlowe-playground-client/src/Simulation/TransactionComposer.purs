module Simulation.TransactionComposer where

import Bootstrap (btn, btnPrimary, card, cardBody_, col6, col_, row_)
import Data.Array as Array
import Data.BigInteger (BigInteger)
import Data.Eq ((/=))
import Data.FunctorWithIndex (mapWithIndex)
import Data.HeytingAlgebra ((||))
import Data.Lens (to, view, (^.))
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Halogen.HTML (ClassName(..), HTML, b_, button, div, h2, h3_, li_, text, ul_)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes, enabled)
import Marlowe.Semantics (AccountId(..), ChoiceId(..), Input(..), Party, PubKey, TransactionError)
import Prelude (const, flip, identity, show, ($), (+), (<<<), (<>), (>))
import Simulation.Common (isContractValid, onEmpty, paneHeader, spanText)
import Types (FrontendState, HAction(..), MarloweState, _Head, _marloweState, _pendingInputs, _transactionError)

transactionComposerPane ::
  forall p.
  FrontendState ->
  HTML p HAction
transactionComposerPane state =
  div
    [ classes
        [ col6
        , ClassName "input-composer"
        ]
    ]
    [ paneHeader "Transaction Composer"
    , div
        [ class_ $ ClassName "wallet"
        ]
        [ div
            [ classes
                ( ( if view (_marloweState <<< _Head <<< _transactionError <<< to isJust) state then
                      (flip Array.snoc) (ClassName "invalid-transaction")
                    else
                      identity
                  )
                    [ card ]
                )
            ]
            [ cardBody_ $ transactionInputs (view (_marloweState <<< _Head) state)
                -- <> ( signatures (view (_marloweState <<< _Head <<< _transaction <<< _signatures) state) (isContractValid state) (view (_marloweState <<< _Head <<< _transaction <<< _outcomes) state)
                
                --   )
                
                <> transactionButtons state
            ]
        ]
    ]

transactionButtons :: FrontendState -> forall p. Array (HTML p HAction)
transactionButtons state =
  [ div
      [ classes
          [ ClassName "d-flex"
          , ClassName "flex-row"
          , ClassName "align-items-center"
          , ClassName "justify-content-start"
          , ClassName "transaction-btn-row"
          ]
      ]
      [ button
          [ classes
              [ btn
              , btnPrimary
              , ClassName "transaction-btn"
              ]
          , onClick $ Just <<< const ApplyTransaction
          , enabled $ isContractValid state
          ]
          [ text "Apply Transaction" ]
      , button
          [ classes
              [ btn
              , btnPrimary
              , ClassName "transaction-btn"
              ]
          , onClick $ Just <<< const NextSlot
          , enabled (isContractValid state)
          ]
          [ text "Next Block" ]
      , button
          [ classes
              [ btn
              , btnPrimary
              , ClassName "transaction-btn"
              ]
          , enabled (hasHistory state)
          , onClick $ Just <<< const ResetSimulator
          ]
          [ text "Reset" ]
      , button
          [ classes
              [ btn
              , btnPrimary
              , ClassName "transaction-btn"
              ]
          , enabled (hasHistory state)
          , onClick $ Just <<< const Undo
          ]
          [ text "Undo" ]
      ]
  ]

hasHistory :: FrontendState -> Boolean
hasHistory state = NEL.length (view _marloweState state) > 1

-- TODO: Need to make these errors nice explanations - function in smeantics utils
printTransError :: forall p. TransactionError -> Array (HTML p HAction)
printTransError error = [ ul_ [ li_ [ text (show error) ] ] ]

transactionErrors :: forall p. Maybe TransactionError -> Array (HTML p HAction)
transactionErrors Nothing = []

transactionErrors (Just error) =
  [ div
      [ classes
          [ ClassName "invalid-transaction"
          , ClassName "input-composer"
          ]
      ]
      ( [ h2 [] [ text "The transaction is invalid:" ] ]
          <> printTransError error
      )
  ]

transactionInputs :: forall p. MarloweState -> Array (HTML p HAction)
transactionInputs state =
  [ h3_
      [ text "Input list"
      ]
  ]
    <> [ div [ class_ $ ClassName "state-row" ]
          ( onEmpty [ text "No inputs in the transaction" ]
              $ mapWithOneIndex (inputRow isEnabled) (state ^. _pendingInputs)
          )
      ]
  where
  isEnabled = state.contract /= Nothing || state.editorErrors /= []

  mapWithOneIndex f = mapWithIndex (\i a -> f (i + 1) a)

inputRow :: forall p. Boolean -> Int -> Tuple Input (Maybe PubKey) -> HTML p HAction
inputRow isEnabled idx (Tuple INotify person) =
  row_
    [ col_
        [ button
            [ class_ $ ClassName "composer-add-button"
            , enabled isEnabled
            , onClick $ const $ Just $ RemoveInput person INotify
            ]
            [ text $ "- " <> show idx <> ":"
            ]
        , text "Notification"
        ]
    ]

inputRow isEnabled idx (Tuple input@(IDeposit accountId party money) person) =
  row_
    [ col_
        $ [ button
              [ class_ $ ClassName "composer-add-button"
              , enabled isEnabled
              , onClick $ const $ Just $ RemoveInput person input
              ]
              [ text $ "- " <> show idx <> ":"
              ]
          ]
        <> (renderDeposit accountId party (unwrap money))
    ]

inputRow isEnabled idx (Tuple input@(IChoice (ChoiceId choiceName choiceOwner) chosenNum) person) =
  row_
    [ col_
        [ button
            [ class_ $ ClassName "composer-add-button"
            , enabled isEnabled
            , onClick $ const $ Just $ RemoveInput person input
            ]
            [ text $ "- " <> show idx <> ":"
            ]
        , text "Participant "
        , b_
            [ text (show choiceOwner)
            ]
        , text " chooses the value "
        , b_
            [ text (show chosenNum)
            ]
        , text " for choice with id "
        , b_
            [ text (show choiceName)
            ]
        ]
    ]

renderDeposit :: forall p. AccountId -> Party -> BigInteger -> Array (HTML p HAction)
renderDeposit (AccountId accountNumber accountOwner) party money =
  [ spanText "Deposit "
  , b_ [ spanText (show money) ]
  , spanText " ADA into Account "
  , b_ [ spanText (show accountOwner <> " (" <> show accountNumber <> ")") ]
  , spanText " as "
  , b_ [ spanText party ]
  ]
