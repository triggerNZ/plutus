module Simulation.Refactoring where

import Bootstrap (btn, btnSecondary)
import Data.Maybe (Maybe(..))
import Halogen.HTML (ClassName(..), HTML, button, div, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (ButtonType(..), classes, type_)
import Halogen.HTML.Properties.ARIA (role)
import Marlowe.Holes (Refactoring(..))
import Marlowe.Semantics (AccountId)
import Prelude (const, ($))
import Simulation.AccountIdEditor (accountIdEditor)
import Types (HAction(..))

refactoringPane :: forall p. Array AccountId -> Boolean -> Maybe Refactoring -> HTML p HAction
refactoringPane accounts display refactoring =
  div
    [ classes [ ClassName "btn-group-vertical", ClassName "w-100" ]
    , role "group"
    ]
    ( [ div [ classes [ btn ] ]
          [ text "Refactorings"
          , displayRefactoring display refactoring
          ]
      ]
    )

displayRefactoring :: forall p. Boolean -> Maybe Refactoring -> HTML p HAction
displayRefactoring _ Nothing = text ""

displayRefactoring false (Just (ExtractAccountId _)) =
  button
    [ classes [ btn, btnSecondary, ClassName "button-box" ]
    , type_ ButtonButton
    , onClick $ const $ Just $ StartRefactoring
    ]
    [ text "Extract Account Id" ]

displayRefactoring true (Just (ExtractAccountId { name, accountId, start, end })) = accountIdEditor name accountId start end
