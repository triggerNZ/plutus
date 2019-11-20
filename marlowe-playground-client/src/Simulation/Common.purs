module Simulation.Common where

import Prelude
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Halogen.HTML (ClassName(..), HTML, h2, span, text)
import Halogen.HTML.Properties (class_)
import Types (FrontendState, HAction, _Head, _contract, _editorErrors, _marloweState)

isContractValid :: FrontendState -> Boolean
isContractValid state =
  view (_marloweState <<< _Head <<< _contract) state /= Nothing
    && view (_marloweState <<< _Head <<< _editorErrors) state
    == []

spanText :: forall p. String -> HTML p HAction
spanText s = span [ class_ $ ClassName "pr-1" ] [ text s ]

paneHeader :: forall p. String -> HTML p HAction
paneHeader s = h2 [ class_ $ ClassName "pane-header" ] [ text s ]

onEmpty :: forall a. Array a -> Array a -> Array a
onEmpty alt [] = alt

onEmpty _ arr = arr
