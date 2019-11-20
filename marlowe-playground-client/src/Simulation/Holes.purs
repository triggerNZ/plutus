module Simulation.Holes where

import Ace.Types as Ace
import Bootstrap (btn, btnSecondary, dropdownToggle)
import Bootstrap.Extra (ariaExpanded, ariaHasPopup, ariaLabelledBy, dataToggle)
import Control.Alternative (map)
import Data.Array (fromFoldable, head, sortBy)
import Data.Array as Array
import Data.Eq ((==))
import Data.Function (on)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Halogen.HTML (ClassName(..), HTML, a, button, div, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (ButtonType(..), class_, classes, id_, type_)
import Halogen.HTML.Properties.ARIA (role)
import Marlowe.Holes (Holes(..), MarloweHole(..), MarloweType(..), getMarloweConstructors)
import Prelude (compare, const, mempty, ($), (<<<), (<>))
import Text.Parsing.Parser.Pos (Position(..))
import Types (HAction(..))

holesPane :: forall p. Maybe String -> Holes -> HTML p HAction
holesPane selectedHole (Holes holes) =
  let
    kvs = Map.toUnfoldable holes

    ordered = sortBy (compare `on` (head <<< snd)) kvs

    holesGroup = map (\(Tuple k v) -> displayHole selectedHole k v) ordered
  in
    div
      [ classes [ ClassName "btn-group-vertical", ClassName "w-100" ]
      , role "group"
      ]
      ( [ div [ classes [ btn ] ] [ text "Holes" ] ]
          <> holesGroup
      )

displayHole :: forall p. Maybe String -> String -> Array MarloweHole -> HTML p HAction
displayHole selectedHole name holes =
  div [ classes ([ ClassName "btn-group" ] <> showClass) ]
    [ button
        [ classes [ btn, btnSecondary, dropdownToggle, ClassName "button-box" ]
        , id_ ("hole-btn-" <> name)
        , type_ ButtonButton
        , dataToggle "dropdown"
        , ariaHasPopup true
        , ariaExpanded expanded
        , onClick $ const $ Just $ SelectHole selectHole
        ]
        [ text name ]
    , div
        [ classes ([ ClassName "dropdown-menu" ] <> showClass)
        , ariaLabelledBy ("hole-btn-" <> name)
        ]
        (holeDropdowns holes)
    ]
  where
  expanded = selectedHole == Just name

  showClass = if selectedHole == Just name then [ ClassName "show" ] else []

  selectHole = if selectedHole == Just name then Nothing else Just name

holeDropdowns :: forall p. Array MarloweHole -> Array (HTML p HAction)
holeDropdowns holes = case Array.uncons holes of
  Nothing -> mempty
  Just { head: (MarloweHole { marloweType: BigIntegerType, end }) } ->
    [ div
        [ classes [ ClassName "dropdown-item", ClassName "font-italic" ]
        , onClick $ const $ Just $ MarloweMoveToPosition $ holeToAcePosition end
        ]
        [ text "Replace the hole with an integer" ]
    ]
  Just { head: (MarloweHole { marloweType: StringType, end }) } ->
    [ div
        [ classes [ ClassName "dropdown-item", ClassName "font-italic" ]
        , onClick $ const $ Just $ MarloweMoveToPosition $ holeToAcePosition end
        ]
        [ text "Replace the hole with a string" ]
    ]
  Just { head: (MarloweHole { marloweType: ValueIdType, end }) } ->
    [ div
        [ classes [ ClassName "dropdown-item", ClassName "font-italic" ]
        , onClick $ const $ Just $ MarloweMoveToPosition $ holeToAcePosition end
        ]
        [ text "Replace the hole with a string" ]
    ]
  Just { head: (MarloweHole { marloweType: SlotType, end }) } ->
    [ div
        [ classes [ ClassName "dropdown-item", ClassName "font-italic" ]
        , onClick $ const $ Just $ MarloweMoveToPosition $ holeToAcePosition end
        ]
        [ text "Replace the hole with an integer" ]
    ]
  Just { head: hole@(MarloweHole { marloweType }) } ->
    map
      ( \constructor ->
          a
            [ class_ $ ClassName "dropdown-item"
            , onClick $ const $ Just $ InsertHole constructor hole holes
            ]
            [ text constructor ]
      )
      (fromFoldable $ Map.keys $ getMarloweConstructors marloweType)
  where
  holeToAcePosition (Position { column, line }) = Ace.Position { column, row: line }
