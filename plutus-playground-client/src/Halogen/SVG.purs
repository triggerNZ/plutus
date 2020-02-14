module Halogen.SVG where

import DOM.HTML.Indexed (Interactive)
import Data.Array as Array
import Data.String (joinWith)
import Halogen.HTML (AttrName(..), ElemName(..), HTML, Namespace(..), Node, elementNS, text)
import Halogen.HTML as HH
import Halogen.HTML.Properties (CSSPixel, IProp)
import Prelude

------------------------------------------------------------
-- Nodes.
------------------------------------------------------------
type SVGNode
  = SVGStyling
      ( SVGCore
          ( viewBox :: Box
          , xmlns :: Namespace
          , width :: CSSPixel
          , height :: CSSPixel
          )
      )

type SVGrect
  = SVGPresentation
      ( Interactive
          ( x :: CSSPixel
          , y :: CSSPixel
          , width :: CSSPixel
          , height :: CSSPixel
          , fill :: RGB
          , stroke :: RGB
          , strokeWidth :: CSSPixel
          )
      )

type SVGg
  = SVGPresentation (SVGCore ())

type SVGCore r
  = ( id :: String | r )

type SVGStyling r
  = ( style :: String | r )

type SVGPresentation r
  = ( transform :: Translate | r )

type SVGline
  = ( x1 :: CSSPixel
    , y1 :: CSSPixel
    , x2 :: CSSPixel
    , y2 :: CSSPixel
    , stroke :: RGB
    , strokeWidth :: CSSPixel
    )

type SVGtext
  = SVGPresentation
      ( x :: CSSPixel
      , y :: CSSPixel
      , stroke :: RGB
      , textAnchor :: Anchor
      , transform :: Translate
      )

svgNS :: Namespace
svgNS = Namespace "http://www.w3.org/2000/svg"

svg :: forall p i. Node SVGNode p i
svg attributes = elementNS svgNS (ElemName "svg") (Array.snoc attributes (xmlns svgNS))

rect :: forall p i. Node SVGrect p i
rect = elementNS svgNS (ElemName "rect")

svgText :: forall r i. Array (IProp SVGtext i) -> String -> HTML r i
svgText attributes content = elementNS svgNS (ElemName "text") attributes [ text content ]

g :: forall p i. Node SVGg p i
g = elementNS svgNS (ElemName "g")

line :: forall p i. Node SVGline p i
line = elementNS svgNS (ElemName "line")

------------------------------------------------------------
-- Attributes.
------------------------------------------------------------
class IsAttr a where
  toAttrValue :: a -> String

instance isAttrNamespace :: IsAttr Namespace where
  toAttrValue (Namespace namespace) = namespace

instance isAttrString :: IsAttr String where
  toAttrValue = identity

instance isAttrInt :: IsAttr Int where
  toAttrValue = show

attr :: forall i r a. IsAttr a => AttrName -> a -> IProp r i
attr name = HH.attr name <<< toAttrValue

x :: forall r i. CSSPixel -> IProp ( x :: CSSPixel | r ) i
x = attr (AttrName "x")

y :: forall r i. CSSPixel -> IProp ( y :: CSSPixel | r ) i
y = attr (AttrName "y")

x1 :: forall r i. CSSPixel -> IProp ( x1 :: CSSPixel | r ) i
x1 = attr (AttrName "x1")

y1 :: forall r i. CSSPixel -> IProp ( y1 :: CSSPixel | r ) i
y1 = attr (AttrName "y1")

x2 :: forall r i. CSSPixel -> IProp ( x2 :: CSSPixel | r ) i
x2 = attr (AttrName "x2")

y2 :: forall r i. CSSPixel -> IProp ( y2 :: CSSPixel | r ) i
y2 = attr (AttrName "y2")

height :: forall r i. CSSPixel -> IProp ( height :: CSSPixel | r ) i
height = attr (AttrName "height")

width :: forall r i. CSSPixel -> IProp ( width :: CSSPixel | r ) i
width = attr (AttrName "width")

xmlns :: forall r i. Namespace -> IProp ( xmlns :: Namespace | r ) i
xmlns = attr (AttrName "xmlns")

transform :: forall r i. Translate -> IProp ( transform :: Translate | r ) i
transform = attr (AttrName "transform")

textAnchor :: forall r i. Anchor -> IProp ( textAnchor :: Anchor | r ) i
textAnchor = attr (AttrName "text-anchor")

fill :: forall r i. RGB -> IProp ( fill :: RGB | r ) i
fill = attr (AttrName "fill")

stroke :: forall r i. RGB -> IProp ( stroke :: RGB | r ) i
stroke = attr (AttrName "stroke")

strokeWidth :: forall r i. CSSPixel -> IProp ( strokeWidth :: CSSPixel | r ) i
strokeWidth = attr (AttrName "stroke-width")

id_ :: forall r i. String -> IProp ( id :: String | r ) i
id_ = attr (AttrName "id")

style :: forall r i. String -> IProp ( style :: String | r ) i
style = attr (AttrName "style")

viewBox :: forall i r. Box -> IProp ( viewBox :: Box | r ) i
viewBox = attr (AttrName "viewBox")

------------------------------------------------------------
-- Types
------------------------------------------------------------
data Box
  = Box
    { x :: Int
    , y :: Int
    , width :: Int
    , height :: Int
    }

instance isAttrBox :: IsAttr Box where
  toAttrValue (Box box) = joinWith " " (show <$> [ box.x, box.y, box.width, box.height ])

data Translate
  = Translate
    { x :: Int
    , y :: Int
    }

instance isAttrTranslate :: IsAttr Translate where
  toAttrValue (Translate translate) = "translate" <> parens (joinWith "," (show <$> [ translate.x, translate.y ]))

data RGB
  = RGB
    { r :: Int
    , g :: Int
    , b :: Int
    }

instance isAttrRGB :: IsAttr RGB where
  toAttrValue (RGB rgb) = "rgb" <> parens (joinWith "," (show <$> [ rgb.r, rgb.g, rgb.b ]))

rgb :: Int -> Int -> Int -> RGB
rgb r g b = RGB { r, g, b }

data Anchor
  = Start
  | Middle
  | End

instance isAttrAnchor :: IsAttr Anchor where
  toAttrValue Start = "start"
  toAttrValue Middle = "middle"
  toAttrValue End = "end"

parens :: String -> String
parens str = "(" <> str <> ")"
