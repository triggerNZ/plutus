module Simulation.Refactoring where

import Bootstrap (btn, btnSecondary)
import Data.Maybe (Maybe(..))
import Halogen.HTML (ClassName(..), HTML, button, div, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (ButtonType(..), classes, type_)
import Halogen.HTML.Properties.ARIA (role)
import Marlowe.Term (Refactoring(..))
import Marlowe.Semantics (AccountId)
import Prelude (const, mempty, ($))
import Types (HAction(..))

refactoringPane :: forall p. Array AccountId -> Maybe Refactoring -> HTML p HAction
refactoringPane accounts refactoring =
  div
    [ classes [ ClassName "btn-group-vertical", ClassName "w-100" ]
    , role "group"
    ]
    ( [ div [ classes [ btn ] ]
          [ text "Refactorings"
          , displayRefactoring refactoring
          ]
      ]
    )

displayRefactoring :: forall p. Maybe Refactoring -> HTML p HAction
displayRefactoring (Just r@(ExtractAccountId _)) =
  button
    [ classes [ btn, btnSecondary, ClassName "button-box" ]
    , type_ ButtonButton
    , onClick $ const $ Just $ Refactor r
    ]
    [ text "Extract Account Id" ]

displayRefactoring _ = text mempty
