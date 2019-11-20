module Simulation.Analysis where

import Bootstrap (btn, btnPrimary, cardBody_, card_, empty)
import Data.Either (Either(..))
import Data.Lens (to, (^.))
import Data.List (List, toUnfoldable, null)
import Data.Maybe (Maybe(..))
import Halogen.HTML (ClassName(..), HTML, PropName(..), b_, button, code_, div, h3_, li_, ol_, span, span_, text, ul_)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes, enabled, prop)
import Marlowe.Parser (transactionInputList, transactionWarningList)
import Marlowe.Semantics (AccountId(..), Ada(..), ChoiceId(..), Input(..), Payee(..), Slot(..), SlotInterval(..), TransactionInput(..), TransactionWarning(..))
import Marlowe.Symbolic.Types.Response as R
import Network.RemoteData (RemoteData(..), isLoading)
import Prelude (bind, const, not, pure, show, ($), (<<<), (<>))
import Simulation.Common (paneHeader, spanText)
import Text.Parsing.Parser (runParser)
import Types (FrontendState, HAction(..), _analysisState)

analysisPane :: forall p. FrontendState -> HTML p HAction
analysisPane state =
  div [ class_ $ ClassName "full-width-card" ]
    [ paneHeader "Static analysis"
    , card_
        [ cardBody_
            [ analysisResultPane state
            , button
                [ classes
                    [ btn
                    , btnPrimary
                    , ClassName "transaction-btn"
                    ]
                , onClick $ const $ Just $ AnalyseContract
                , enabled $ state ^. _analysisState <<< to (not isLoading)
                ]
                [ loading
                , text btnText
                ]
            ]
        ]
    ]
  where
  btnText = case state ^. _analysisState of
    Loading -> "  Analysing..."
    _ -> "Analyse Contract"

  loading = case state ^. _analysisState of
    Loading ->
      span
        [ classes
            [ ClassName "spinner-border"
            , ClassName "spinner-border-sm"
            ]
        , prop (PropName "role") "status"
        , prop (PropName "aria-hidden") "true"
        ]
        []
    _ -> empty

analysisResultPane :: forall p. FrontendState -> HTML p HAction
analysisResultPane state =
  let
    result = state ^. _analysisState
  in
    case result of
      NotAsked ->
        div [ classes [ ClassName "padded-explanation" ] ]
          [ text "Press the button below to analyse the contract for runtime warnings." ]
      Success (R.Valid) ->
        div [ classes [ ClassName "padded-explanation" ] ]
          [ h3_ [ text "Analysis Result: Pass" ]
          , text "Static analysis could not find any execution that results in any warning."
          ]
      Success (R.CounterExample { initialSlot, transactionList, transactionWarning }) ->
        div [ classes [ ClassName "padded-explanation" ] ]
          [ h3_ [ text "Analysis Result: Fail" ]
          , text "Static analysis found the following counterexample:"
          , ul_
              [ li_
                  [ spanText "Initial slot: "
                  , b_ [ spanText (show initialSlot) ]
                  ]
              , li_
                  [ spanText "Offending transaction list: "
                  , displayTransactionList transactionList
                  ]
              , li_
                  [ spanText "Warnings issued: "
                  , displayWarningList transactionWarning
                  ]
              ]
          ]
      Success (R.Error str) ->
        div [ classes [ ClassName "padded-explanation" ] ]
          [ h3_ [ text "Error during analysis" ]
          , text "Analysis failed for the following reason:"
          , ul_
              [ li_
                  [ b_ [ spanText str ]
                  ]
              ]
          ]
      Failure failure ->
        div [ classes [ ClassName "padded-explanation" ] ]
          [ h3_ [ text "Error during analysis" ]
          , text "Analysis failed for the following reason:"
          , ul_
              [ li_
                  [ b_ [ spanText failure ]
                  ]
              ]
          ]
      _ -> empty

displayTransactionList :: forall p. String -> HTML p HAction
displayTransactionList transactionList = case runParser transactionList transactionInputList of
  Right pTL ->
    ol_
      ( do
          ( TransactionInput
              { interval: SlotInterval (Slot from) (Slot to)
            , inputs: inputList
            }
          ) <-
            ((toUnfoldable pTL) :: Array TransactionInput)
          pure
            ( li_
                [ span_
                    [ b_ [ text "Transaction" ]
                    , text " with slot interval "
                    , b_ [ text $ (show from <> " to " <> show to) ]
                    , if null inputList then
                        text " and no inputs (empty transaction)."
                      else
                        text " and inputs:"
                    ]
                , if null inputList then
                    empty
                  else
                    displayInputList inputList
                ]
            )
      )
  Left _ -> code_ [ text transactionList ]

displayInputList :: forall p. List Input -> HTML p HAction
displayInputList inputList =
  ol_
    ( do
        input <- (toUnfoldable inputList)
        pure (li_ (displayInput input))
    )

displayInput :: forall p. Input -> Array (HTML p HAction)
displayInput (IDeposit (AccountId accNum owner) party (Lovelace money)) =
  [ b_ [ text "IDeposit" ]
  , text " - Party "
  , b_ [ text $ show party ]
  , text " deposits "
  , b_ [ text ((show money) <> " Lovelace") ]
  , text " into account "
  , b_ [ text ((show accNum) <> " of " <> (show owner)) ]
  , text "."
  ]

displayInput (IChoice (ChoiceId choiceId party) chosenNum) =
  [ b_ [ text "IChoice" ]
  , text " - Party "
  , b_ [ text $ show party ]
  , text " chooses number "
  , b_ [ text $ show chosenNum ]
  , text " for choice "
  , b_ [ text $ show choiceId ]
  , text "."
  ]

displayInput (INotify) =
  [ b_ [ text "INotify" ]
  , text " - The contract is notified that an observation became "
  , b_ [ text "True" ]
  ]

displayWarningList :: forall p. String -> HTML p HAction
displayWarningList transactionWarnings = case runParser transactionWarnings transactionWarningList of
  Right pWL ->
    ol_
      ( do
          warning <- ((toUnfoldable pWL) :: Array TransactionWarning)
          pure (li_ (displayWarning warning))
      )
  Left _ -> code_ [ text transactionWarnings ]

displayWarning :: forall p. TransactionWarning -> Array (HTML p HAction)
displayWarning (TransactionNonPositiveDeposit party (AccountId accNum owner) (Lovelace amount)) =
  [ b_ [ text "TransactionNonPositiveDeposit" ]
  , text " - Party "
  , b_ [ text $ show party ]
  , text " is asked to deposit "
  , b_ [ text ((show amount) <> " Lovelace") ]
  , text " into account "
  , b_ [ text ((show accNum) <> " of " <> (show owner)) ]
  , text "."
  ]

displayWarning (TransactionNonPositivePay (AccountId accNum owner) payee (Lovelace amount)) =
  [ b_ [ text "TransactionNonPositivePay" ]
  , text " - The contract is suppoused to make a payment of "
  , b_ [ text ((show amount) <> " Lovelace") ]
  , text " from account "
  , b_ [ text ((show accNum) <> " of " <> (show owner)) ]
  , text " to "
  , b_
      [ text case payee of
          (Account (AccountId accNum2 owner2)) -> ("account " <> (show accNum2) <> " of " <> (show owner2))
          (Party dest) -> ("party " <> (show dest))
      ]
  , text "."
  ]

displayWarning (TransactionPartialPay (AccountId accNum owner) payee (Lovelace amount) (Lovelace expected)) =
  [ b_ [ text "TransactionPartialPay" ]
  , text " - The contract is suppoused to make a payment of "
  , b_ [ text ((show expected) <> " Lovelace") ]
  , text " from account "
  , b_ [ text ((show accNum) <> " of " <> (show owner)) ]
  , text " to "
  , b_
      [ text case payee of
          (Account (AccountId accNum2 owner2)) -> ("account " <> (show accNum2) <> " of " <> (show owner2))
          (Party dest) -> ("party " <> (show dest))
      ]
  , text " but there is only "
  , b_ [ text ((show amount) <> " Lovelace") ]
  , text "."
  ]

displayWarning (TransactionShadowing valId oldVal newVal) =
  [ b_ [ text "TransactionShadowing" ]
  , text " - The contract defined the value with id "
  , b_ [ text (show valId) ]
  , text " before, it was assigned the value "
  , b_ [ text (show oldVal) ]
  , text " and now it is being assigned the value "
  , b_ [ text (show newVal) ]
  , text "."
  ]
