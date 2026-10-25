-- | Attributes for Lynx elements and the supported SVG markup subset
module Flame.Html.Attribute.Internal (class ToClassList, class ToStyleList, ToBooleanAttribute, ToIntAttribute, ToNumberAttribute, ToStringAttribute, class', createAttribute, createProperty, style, style1, to, toStyleList, key, autoSize, autoplay, id, content, placeholder, src, type', value, height, maxlength, width, clipPathAttr, clipPathUnits, clipRule, color, cx, cy, d, fill, fillOpacity, fillRule, fontSize, fx, fy, gradientTransform, gradientUnits, href, opacity, preserveAspectRatio, r, rx, ry, stopColor, stopOpacity, stroke, strokeDasharray, strokeDashoffset, strokeLinecap, strokeLinejoin, strokeMiterlimit, strokeOpacity, strokeWidth, transform, viewBox, x, y, x1, y1, x2, y2, points, mode, styleAttr, disabled, readOnly, title, target, rel, action, method, accept, autocomplete, alt, charset, for, lang, name, checked, selected, rows, scrollOrientation) where

import Data.Array as DA
import Data.Either as DE
import Data.Foldable as DF
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Data.String.Regex as DSR
import Data.String.Regex.Flags (global)
import Data.Tuple (Tuple(..))
import Flame.Types (NodeData, ToNodeData)
import Foreign.Object (Object)
import Foreign.Object as FO
import Partial as P
import Partial.Unsafe as PU
import Prelude (const, flip, identity, map, not, otherwise, show, ($), (<<<), (<>), (==))
import Type.Row.Homogeneous (class Homogeneous)

type ToStringAttribute = ToNodeData String

type ToIntAttribute = ToNodeData Int

type ToBooleanAttribute = ToNodeData Boolean

type ToNumberAttribute = ToNodeData Number

-- | Enables either strings or records be used as an argument to `class'`
class ToClassList a where
      to ∷ a → Array String

instance ToClassList String where
      to = DA.filter (not <<< DS.null) <<< DS.split (Pattern " ")

instance ToClassList (Array String) where
      to = identity

instance Homogeneous r Boolean ⇒ ToClassList { | r } where
      to = FO.keys <<< FO.filterWithKey (flip const) <<< FO.fromHomogeneous

-- | Enables either tuples, arrays or records be used as an argument to `style`
class ToStyleList a where
      toStyleList ∷ a → Object String

instance ToStyleList (Tuple String String) where
      toStyleList (Tuple a b) = FO.singleton a b
else instance Homogeneous r String ⇒ ToStyleList { | r } where
      toStyleList = FO.fromFoldable <<< map go <<< toArray
            where
            go (Tuple name' value') = Tuple (caseify name') value'

            toArray ∷ _ → Array (Tuple String String)
            toArray = FO.toUnfoldable <<< FO.fromHomogeneous
else instance DF.Foldable f ⇒ ToStyleList (f (Tuple String String)) where
      toStyleList = FO.fromFoldable

foreign import createProperty ∷ ∀ message. String → String → NodeData message

foreign import createClass ∷ ∀ message. Array String → NodeData message

foreign import createStyle ∷ ∀ message. Object String → NodeData message

class' ∷ ∀ a b. ToClassList b ⇒ b → NodeData a
class' = createClass <<< map caseify <<< to

createAttribute ∷ ∀ message. String → String → NodeData message
createAttribute = createProperty

-- | Sets the node style
-- |
-- | Use styles for Lynx layout and visual properties
style ∷ ∀ a r. ToStyleList r ⇒ r → NodeData a
style record = createStyle $ toStyleList record

style1 ∷ ∀ a. String → String → NodeData a
style1 a b = createStyle $ FO.singleton a b

-- | Transforms its input into a proper html attribute/tag name, i.e. lower case and hyphenated
caseify ∷ String → String
caseify name'
      | name' == DS.toUpper name' = DS.toLower name'
      | otherwise = DS.toLower (DS.singleton head) <> hyphenated
              where
              { head, tail } = PU.unsafePartial (DM.fromJust $ DS.uncons name')

              regex = PU.unsafePartial case DSR.regex "[A-Z]" global of
                    DE.Right rgx → rgx
                    DE.Left err → P.crashWith $ show err

              replacer = const <<< ("-" <> _) <<< DS.toLower

              hyphenated = DSR.replace' regex replacer tail

foreign import createBooleanProp ∷ ∀ message. String → Boolean → NodeData message

foreign import createIntProp ∷ ∀ message. String → Int → NodeData message

key ∷ ToStringAttribute
key = createProperty "key"

autoSize ∷ ToBooleanAttribute
autoSize = createBooleanProp "auto-size"

id ∷ ToStringAttribute
id = createProperty "id"

content ∷ ToStringAttribute
content = createProperty "content"

placeholder ∷ ToStringAttribute
placeholder = createProperty "placeholder"

src ∷ ToStringAttribute
src = createProperty "src"

type' ∷ ToStringAttribute
type' = createProperty "type"

value ∷ ToStringAttribute
value = createProperty "value"

height ∷ ToStringAttribute
height = createProperty "height"

maxlength ∷ ToIntAttribute
maxlength = createIntProp "maxlength"

width ∷ ToStringAttribute
width = createProperty "width"

clipPathAttr ∷ ToStringAttribute
clipPathAttr = createProperty "clip-path"

clipPathUnits ∷ ToStringAttribute
clipPathUnits = createProperty "clipPathUnits"

clipRule ∷ ToStringAttribute
clipRule = createProperty "clip-rule"

color ∷ ToStringAttribute
color = createProperty "color"

cx ∷ ToStringAttribute
cx = createProperty "cx"

cy ∷ ToStringAttribute
cy = createProperty "cy"

d ∷ ToStringAttribute
d = createProperty "d"

fill ∷ ToStringAttribute
fill = createProperty "fill"

fillOpacity ∷ ToStringAttribute
fillOpacity = createProperty "fill-opacity"

fillRule ∷ ToStringAttribute
fillRule = createProperty "fill-rule"

fontSize ∷ ToStringAttribute
fontSize = createProperty "font-size"

fx ∷ ToStringAttribute
fx = createProperty "fx"

fy ∷ ToStringAttribute
fy = createProperty "fy"

gradientTransform ∷ ToStringAttribute
gradientTransform = createProperty "gradientTransform"

gradientUnits ∷ ToStringAttribute
gradientUnits = createProperty "gradientUnits"

href ∷ ToStringAttribute
href = createProperty "href"

opacity ∷ ToStringAttribute
opacity = createProperty "opacity"

preserveAspectRatio ∷ ToStringAttribute
preserveAspectRatio = createProperty "preserveAspectRatio"

r ∷ ToStringAttribute
r = createProperty "r"

rx ∷ ToStringAttribute
rx = createProperty "rx"

ry ∷ ToStringAttribute
ry = createProperty "ry"

stopColor ∷ ToStringAttribute
stopColor = createProperty "stop-color"

stopOpacity ∷ ToStringAttribute
stopOpacity = createProperty "stop-opacity"

stroke ∷ ToStringAttribute
stroke = createProperty "stroke"

strokeDasharray ∷ ToStringAttribute
strokeDasharray = createProperty "stroke-dasharray"

strokeDashoffset ∷ ToStringAttribute
strokeDashoffset = createProperty "stroke-dashoffset"

strokeLinecap ∷ ToStringAttribute
strokeLinecap = createProperty "stroke-linecap"

strokeLinejoin ∷ ToStringAttribute
strokeLinejoin = createProperty "stroke-linejoin"

strokeMiterlimit ∷ ToNumberAttribute
strokeMiterlimit = createProperty "stroke-miterlimit" <<< show

strokeOpacity ∷ ToStringAttribute
strokeOpacity = createProperty "stroke-opacity"

strokeWidth ∷ ToStringAttribute
strokeWidth = createProperty "stroke-width"

transform ∷ ToStringAttribute
transform = createProperty "transform"

viewBox ∷ ToStringAttribute
viewBox = createProperty "viewBox"

x ∷ ToStringAttribute
x = createProperty "x"

y ∷ ToStringAttribute
y = createProperty "y"

x1 ∷ ToStringAttribute
x1 = createProperty "x1"

y1 ∷ ToStringAttribute
y1 = createProperty "y1"

x2 ∷ ToStringAttribute
x2 = createProperty "x2"

y2 ∷ ToStringAttribute
y2 = createProperty "y2"

points ∷ ToStringAttribute
points = createProperty "points"

mode ∷ ToStringAttribute
mode = createProperty "mode"

styleAttr ∷ ToStringAttribute
styleAttr = createProperty "style"

disabled ∷ ToBooleanAttribute
disabled = createBooleanProp "disabled"

readOnly ∷ ToBooleanAttribute
readOnly = createBooleanProp "readonly"

autoplay ∷ ToBooleanAttribute
autoplay = createBooleanProp "autoplay"

title ∷ ToStringAttribute
title = createProperty "title"

target ∷ ToStringAttribute
target = createProperty "target"

rel ∷ ToStringAttribute
rel = createProperty "rel"

action ∷ ToStringAttribute
action = createProperty "action"

method ∷ ToStringAttribute
method = createProperty "method"

accept ∷ ToStringAttribute
accept = createProperty "accept"

autocomplete ∷ ToStringAttribute
autocomplete = createProperty "autocomplete"

alt ∷ ToStringAttribute
alt = createProperty "alt"

charset ∷ ToStringAttribute
charset = createProperty "charset"

for ∷ ToStringAttribute
for = createProperty "for"

lang ∷ ToStringAttribute
lang = createProperty "lang"

name ∷ ToStringAttribute
name = createProperty "name"

checked ∷ ToBooleanAttribute
checked = createBooleanProp "checked"

selected ∷ ToBooleanAttribute
selected = createBooleanProp "selected"

rows ∷ ToIntAttribute
rows = createIntProp "rows"

scrollOrientation ∷ ToStringAttribute
scrollOrientation = createProperty "scroll-orientation"
