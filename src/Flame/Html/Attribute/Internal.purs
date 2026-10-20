-- | Attributes for Lynx elements and the supported SVG markup subset
module Flame.Html.Attribute.Internal (class ToClassList, class ToStyleList, ToBooleanAttribute, ToIntAttribute, ToNumberAttribute, ToStringAttribute, class', createAttribute, createProp, style, style1, to, toStyleList, key, autoSize, autoplay, id, content, placeholder, src, type', value, height, maxlength, width, clipPathAttr, clipPathUnits, clipRule, color, cx, cy, d, fill, fillOpacity, fillRule, fontSize, fx, fy, gradientTransform, gradientUnits, href, opacity, preserveAspectRatio, r, rx, ry, stopColor, stopOpacity, stroke, strokeDasharray, strokeDashoffset, strokeLinecap, strokeLinejoin, strokeMiterlimit, strokeOpacity, strokeWidth, transform, viewBox, x, y, x1, y1, x2, y2, points, mode, styleAttr, disabled, readOnly) where

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

foreign import createProp ∷ ∀ message. String → String → NodeData message

foreign import createClass ∷ ∀ message. Array String → NodeData message

foreign import createStyle ∷ ∀ message. Object String → NodeData message

class' ∷ ∀ a b. ToClassList b ⇒ b → NodeData a
class' = createClass <<< map caseify <<< to

createAttribute ∷ ∀ message. String → String → NodeData message
createAttribute = createProp

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
key = createProp "key"

autoSize ∷ ToBooleanAttribute
autoSize = createBooleanProp "auto-size"

id ∷ ToStringAttribute
id = createProp "id"

content ∷ ToStringAttribute
content = createProp "content"

placeholder ∷ ToStringAttribute
placeholder = createProp "placeholder"

src ∷ ToStringAttribute
src = createProp "src"

type' ∷ ToStringAttribute
type' = createProp "type"

value ∷ ToStringAttribute
value = createProp "value"

height ∷ ToStringAttribute
height = createProp "height"

maxlength ∷ ToIntAttribute
maxlength = createIntProp "maxlength"

width ∷ ToStringAttribute
width = createProp "width"

clipPathAttr ∷ ToStringAttribute
clipPathAttr = createProp "clip-path"

clipPathUnits ∷ ToStringAttribute
clipPathUnits = createProp "clipPathUnits"

clipRule ∷ ToStringAttribute
clipRule = createProp "clip-rule"

color ∷ ToStringAttribute
color = createProp "color"

cx ∷ ToStringAttribute
cx = createProp "cx"

cy ∷ ToStringAttribute
cy = createProp "cy"

d ∷ ToStringAttribute
d = createProp "d"

fill ∷ ToStringAttribute
fill = createProp "fill"

fillOpacity ∷ ToStringAttribute
fillOpacity = createProp "fill-opacity"

fillRule ∷ ToStringAttribute
fillRule = createProp "fill-rule"

fontSize ∷ ToStringAttribute
fontSize = createProp "font-size"

fx ∷ ToStringAttribute
fx = createProp "fx"

fy ∷ ToStringAttribute
fy = createProp "fy"

gradientTransform ∷ ToStringAttribute
gradientTransform = createProp "gradientTransform"

gradientUnits ∷ ToStringAttribute
gradientUnits = createProp "gradientUnits"

href ∷ ToStringAttribute
href = createProp "href"

opacity ∷ ToStringAttribute
opacity = createProp "opacity"

preserveAspectRatio ∷ ToStringAttribute
preserveAspectRatio = createProp "preserveAspectRatio"

r ∷ ToStringAttribute
r = createProp "r"

rx ∷ ToStringAttribute
rx = createProp "rx"

ry ∷ ToStringAttribute
ry = createProp "ry"

stopColor ∷ ToStringAttribute
stopColor = createProp "stop-color"

stopOpacity ∷ ToStringAttribute
stopOpacity = createProp "stop-opacity"

stroke ∷ ToStringAttribute
stroke = createProp "stroke"

strokeDasharray ∷ ToStringAttribute
strokeDasharray = createProp "stroke-dasharray"

strokeDashoffset ∷ ToStringAttribute
strokeDashoffset = createProp "stroke-dashoffset"

strokeLinecap ∷ ToStringAttribute
strokeLinecap = createProp "stroke-linecap"

strokeLinejoin ∷ ToStringAttribute
strokeLinejoin = createProp "stroke-linejoin"

strokeMiterlimit ∷ ToNumberAttribute
strokeMiterlimit = createProp "stroke-miterlimit" <<< show

strokeOpacity ∷ ToStringAttribute
strokeOpacity = createProp "stroke-opacity"

strokeWidth ∷ ToStringAttribute
strokeWidth = createProp "stroke-width"

transform ∷ ToStringAttribute
transform = createProp "transform"

viewBox ∷ ToStringAttribute
viewBox = createProp "viewBox"

x ∷ ToStringAttribute
x = createProp "x"

y ∷ ToStringAttribute
y = createProp "y"

x1 ∷ ToStringAttribute
x1 = createProp "x1"

y1 ∷ ToStringAttribute
y1 = createProp "y1"

x2 ∷ ToStringAttribute
x2 = createProp "x2"

y2 ∷ ToStringAttribute
y2 = createProp "y2"

points ∷ ToStringAttribute
points = createProp "points"

mode ∷ ToStringAttribute
mode = createProp "mode"

styleAttr ∷ ToStringAttribute
styleAttr = createProp "style"

disabled ∷ ToBooleanAttribute
disabled = createBooleanProp "disabled"

readOnly ∷ ToBooleanAttribute
readOnly = createBooleanProp "readonly"

autoplay ∷ ToBooleanAttribute
autoplay = createBooleanProp "autoplay"
