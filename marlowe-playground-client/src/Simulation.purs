module Simulation where

import API (RunResult(RunResult))
import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Halogen.Component (Autocomplete(Live), aceComponent)
import Ace.Types (Editor)
import Bootstrap (btn, btnInfo, btnSmall, col3, col9, empty, listGroupItem_, listGroup_, row_)
import Control.Alternative ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Lens (to, view, (^.))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen.HTML (ClassName(..), ComponentHTML, HTML, br_, button, code_, div, div_, pre_, slot, strong_, text)
import Halogen.HTML.Events (onClick, onDragOver, onDrop, onKeyUp)
import Halogen.HTML.Properties (class_, classes, enabled)
import Halogen.HTML.Properties.ARIA (role)
import LocalStorage as LocalStorage
import Marlowe.Semantics (AccountId)
import Prelude (Unit, bind, const, discard, pure, unit, void, ($), (<$>), (<<<), (<>))
import Simulation.Analysis (analysisPane)
import Simulation.Common (isContractValid, paneHeader)
import Simulation.Holes (holesPane)
import Simulation.InputComposer (inputComposerPane)
import Simulation.Refactoring (refactoringPane)
import Simulation.StateTable (statePane, stateTitle)
import Simulation.TransactionComposer (transactionComposerPane, transactionErrors)
import StaticData as StaticData
import Types (ChildSlots, FrontendState, HAction(..), MarloweError(..), _Head, _displayRefactoring, _holes, _marloweAccounts, _marloweCompileResult, _marloweEditorSlot, _marloweState, _refactoring, _selectedHole, _transactionError)

simulationPane ::
  forall m.
  MonadAff m =>
  FrontendState ->
  ComponentHTML HAction ChildSlots m
simulationPane state =
  div_
    ( Array.concat
        [ [ row_
              [ inputComposerPane state
              , transactionComposerPane state
              ]
          , stateTitle state
          , row_ [ statePane state ]
          ]
        , state ^. (_marloweState <<< _Head <<< _transactionError <<< to transactionErrors)
        , [ div
              [ classes
                  [ ClassName "demos"
                  , ClassName "d-flex"
                  , ClassName "flex-row"
                  , ClassName "align-items-center"
                  , ClassName "justify-content-between"
                  , ClassName "mt-5"
                  , ClassName "mb-3"
                  ]
              ]
              [ paneHeader "Marlowe Contract", codeToBlocklyButton state, demoScriptsPane ]
          , div
              []
              [ row_
                  [ div
                      [ class_ col9
                      , onDragOver $ Just <<< MarloweHandleDragEvent
                      , onDrop $ Just <<< MarloweHandleDropEvent
                      , onKeyUp $ Just <<< const MarloweEditorCursorMoved
                      , onClick $ Just <<< const MarloweEditorCursorMoved
                      ]
                      [ slot _marloweEditorSlot unit (aceComponent initEditor (Just Live)) unit (Just <<< MarloweHandleEditorMessage) ]
                  , div [ class_ col3 ]
                      [ holesPane (view _selectedHole state) (view (_marloweState <<< _Head <<< _holes) state)
                      , accountsPane (view (_marloweState <<< _Head <<< _marloweAccounts) state)
                      , refactoringPane (view (_marloweState <<< _Head <<< _marloweAccounts) state) (view _displayRefactoring state) (view (_marloweState <<< _Head <<< _refactoring) state)
                      ]
                  ]
              ]
          , br_
          , errorList
          , analysisPane state
          ]
        ]
    )
  where
  errorList = case view _marloweCompileResult state of
    Left errors -> listGroup_ (listGroupItem_ <<< pure <<< compilationErrorPane <$> errors)
    _ -> empty

loadBuffer :: Effect (Maybe String)
loadBuffer = LocalStorage.getItem StaticData.marloweBufferLocalStorageKey

initEditor ::
  forall m.
  MonadAff m =>
  Editor ->
  m Unit
initEditor editor =
  liftEffect
    $ do
        savedContents <- liftEffect loadBuffer
        let
          defaultContents = Map.lookup "Deposit Incentive" StaticData.marloweContracts
        let
          contents = fromMaybe "" (savedContents <|> defaultContents)
        void $ Editor.setValue contents (Just 1) editor
        Editor.setTheme "ace/theme/monokai" editor
        session <- Editor.getSession editor
        Session.setMode "ace/mode/haskell" session

accountsPane :: forall p. Array AccountId -> HTML p HAction
accountsPane accounts =
  div
    [ classes [ ClassName "btn-group-vertical", ClassName "w-100" ]
    , role "group"
    ]
    ( [ div [ classes [ btn ] ] [ text "Accounts" ] ]
        <> []
    )

demoScriptsPane :: forall p. HTML p HAction
demoScriptsPane =
  div_
    ( Array.cons
        ( strong_
            [ text "Demos: "
            ]
        )
        (demoScriptButton <$> Array.fromFoldable (Map.keys StaticData.marloweContracts))
    )

demoScriptButton :: forall p. String -> HTML p HAction
demoScriptButton key =
  button
    [ classes [ btn, btnInfo, btnSmall ]
    , onClick $ const $ Just $ LoadMarloweScript key
    ]
    [ text key ]

codeToBlocklyButton :: forall p. FrontendState -> HTML p HAction
codeToBlocklyButton state =
  button
    [ classes [ btn, btnInfo, btnSmall ]
    , onClick $ const $ Just $ SetBlocklyCode
    , enabled (isContractValid state)
    ]
    [ text "Code to Blockly" ]

compilationResultPane :: forall p. RunResult -> HTML p HAction
compilationResultPane (RunResult stdout) = div_ [ code_ [ pre_ [ text stdout ] ] ]

compilationErrorPane :: forall p. MarloweError -> HTML p HAction
compilationErrorPane (MarloweError error) = div_ [ text error ]
