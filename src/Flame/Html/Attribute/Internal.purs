-- | Definition of HTML attributes
module Flame.Html.Attribute.Internal (class ToClassList, class ToStyleList, ToBooleanAttribute, ToIntAttribute, ToNumberAttribute, ToStringAttribute, accentHeight, accept, acceptCharset, accessKey, accumulate, action, additive, align, alignmentBaseline, alt, ascent, autocomplete, autofocus, autoplay, azimuth, baseFrequency, baseProfile, baselineShift, begin, bias, calcMode, charset, checked, class', clipPathAttr, clipPathUnits, clipRule, color, colorInterpolation, colorInterpolationFilters, colorProfileAttr, colorRendering, cols, colspan, content, contentEditable, contentScriptType, contentStyleType, contextmenu, controls, coords, createProp, cursorAttr, cx, cy, d, datetime, default, diffuseConstant, dir, direction, disabled, display, divisor, dominantBaseline, download, downloadAs, draggable, dropzone, dur, dx, dy, edgeMode, elevation, enctype, end, externalResourcesRequired, fill, fillOpacity, fillRule, filterAttr, filterUnits, floodColor, floodOpacity, fontFamily, fontSize, fontSizeAdjust, fontStretch, fontStyle, fontVariant, fontWeight, for, fr, from, fx, fy, gradientTransform, gradientUnits, headers, height, hidden, href, hreflang, id, imageRendering, in', in2, isMap, itemprop, k1, k2, k3, k4, kernelMatrix, kernelUnitLength, kerning, key, keySplines, keyTimes, kind, lang, lengthAdjust, letterSpacing, lightingColor, limitingConeAngle, list, local, loop, manifest, markerEnd, markerHeight, markerMid, markerStart, markerUnits, markerWidth, maskAttr, maskContentUnits, maskUnits, max, maxlength, media, method, min, minlength, mode, multiple, name, noValidate, numOctaves, opacity, operator, order, overflow, overlinePosition, overlineThickness, paintOrder, pathLength, pattern, patternContentUnits, patternTransform, patternUnits, ping, placeholder, pointerEvents, points, pointsAtX, pointsAtY, pointsAtZ, poster, preload, preserveAlpha, preserveAspectRatio, primitiveUnits, pubdate, r, radius, readOnly, refX, refY, rel, repeatCount, repeatDur, required, requiredFeatures, restart, result, reversed, rows, rowspan, rx, ry, sandbox, scale, scope, seed, selected, shape, shapeRendering, size, specularConstant, specularExponent, spellcheck, src, srcdoc, srclang, start, stdDeviation, step, stitchTiles, createAttribute, stopColor, stopOpacity, strikethroughPosition, strikethroughThickness, stroke, strokeDasharray, strokeDashoffset, strokeLinecap, strokeLinejoin, strokeMiterlimit, strokeOpacity, strokeWidth, style, style1, styleAttr, surfaceScale, tabindex, target, targetX, targetY, textAnchor, textDecoration, textLength, textRendering, title, to, toStyleList, transform, type', underlinePosition, underlineThickness, useMap, value, values, vectorEffect, version, viewBox, visibility, width, wordSpacing, wrap, writingMode, x, x1, x2, xChannelSelector, y, y1, y2, yChannelSelector, innerHtml) where

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

booleanToFalsyString ∷ Boolean → String
booleanToFalsyString =
      case _ of
            true → "true"
            false → ""

class' ∷ ∀ a b. ToClassList b ⇒ b → NodeData a
class' = createClass <<< map caseify <<< to

createAttribute ∷ ∀ message. String → String → NodeData message
createAttribute = createProp

-- | Sets the node style
-- |
-- | https://developer.mozilla.org/en-US/docs/Web/API/ElementCSSInlineStyle/style
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

-- | Set the key attribute for "keyed" rendering
key ∷ ToStringAttribute
key = createProp "key"

--script generated

id ∷ ToStringAttribute
id = createProp "id"

innerHtml ∷ ToStringAttribute
innerHtml = createProp "innerHTML"

content ∷ ToStringAttribute
content = createProp "content"

accept ∷ ToStringAttribute
accept = createProp "accept"

acceptCharset ∷ ToStringAttribute
acceptCharset = createProp "acceptCharset"

accessKey ∷ ToStringAttribute
accessKey = createProp "accessKey"

action ∷ ToStringAttribute
action = createProp "action"

align ∷ ToStringAttribute
align = createProp "align"

alt ∷ ToStringAttribute
alt = createProp "alt"

charset ∷ ToStringAttribute
charset = createProp "charset"

coords ∷ ToStringAttribute
coords = createProp "coords"

dir ∷ ToStringAttribute
dir = createProp "dir"

download ∷ ToStringAttribute
download = createProp "download"

downloadAs ∷ ToStringAttribute
downloadAs = createProp "downloadAs"

dropzone ∷ ToStringAttribute
dropzone = createProp "dropzone"

enctype ∷ ToStringAttribute
enctype = createProp "enctype"

for ∷ ToStringAttribute
for = createProp "for"

headers ∷ ToStringAttribute
headers = createProp "headers"

href ∷ ToStringAttribute
href = createProp "href"

hreflang ∷ ToStringAttribute
hreflang = createProp "hreflang"

kind ∷ ToStringAttribute
kind = createProp "kind"

lang ∷ ToStringAttribute
lang = createProp "lang"

max ∷ ToStringAttribute
max = createProp "max"

method ∷ ToStringAttribute
method = createProp "method"

min ∷ ToStringAttribute
min = createProp "min"

name ∷ ToStringAttribute
name = createProp "name"

pattern ∷ ToStringAttribute
pattern = createProp "pattern"

ping ∷ ToStringAttribute
ping = createProp "ping"

placeholder ∷ ToStringAttribute
placeholder = createProp "placeholder"

poster ∷ ToStringAttribute
poster = createProp "poster"

preload ∷ ToStringAttribute
preload = createProp "preload"

sandbox ∷ ToStringAttribute
sandbox = createProp "sandbox"

scope ∷ ToStringAttribute
scope = createProp "scope"

shape ∷ ToStringAttribute
shape = createProp "shape"

src ∷ ToStringAttribute
src = createProp "src"

srcdoc ∷ ToStringAttribute
srcdoc = createProp "srcdoc"

srclang ∷ ToStringAttribute
srclang = createProp "srclang"

step ∷ ToStringAttribute
step = createProp "step"

target ∷ ToStringAttribute
target = createProp "target"

title ∷ ToStringAttribute
title = createProp "title"

type' ∷ ToStringAttribute
type' = createProp "type"

useMap ∷ ToStringAttribute
useMap = createProp "useMap"

value ∷ ToStringAttribute
value = createProp "value"

wrap ∷ ToStringAttribute
wrap = createProp "wrap"

cols ∷ ToIntAttribute
cols = createProp "cols" <<< show

colspan ∷ ToIntAttribute
colspan = createProp "colspan" <<< show

height ∷ ToStringAttribute
height = createProp "height"

maxlength ∷ ToIntAttribute
maxlength = createProp "maxlength" <<< show

minlength ∷ ToIntAttribute
minlength = createProp "minlength" <<< show

rows ∷ ToIntAttribute
rows = createProp "rows" <<< show

rowspan ∷ ToIntAttribute
rowspan = createProp "rowspan" <<< show

size ∷ ToIntAttribute
size = createProp "size" <<< show

start ∷ ToIntAttribute
start = createProp "start" <<< show

tabindex ∷ ToIntAttribute
tabindex = createProp "tabIndex" <<< show

width ∷ ToStringAttribute
width = createProp "width"

contextmenu ∷ ToStringAttribute
contextmenu = createProp "contextmenu"

datetime ∷ ToStringAttribute
datetime = createProp "datetime"

draggable ∷ ToStringAttribute
draggable = createProp "draggable"

itemprop ∷ ToStringAttribute
itemprop = createProp "itemprop"

list ∷ ToStringAttribute
list = createProp "list"

manifest ∷ ToStringAttribute
manifest = createProp "manifest"

media ∷ ToStringAttribute
media = createProp "media"

pubdate ∷ ToStringAttribute
pubdate = createProp "pubdate"

rel ∷ ToStringAttribute
rel = createProp "rel"

cx ∷ ToStringAttribute
cx = createProp "cx"

cy ∷ ToStringAttribute
cy = createProp "cy"

fillOpacity ∷ ToStringAttribute
fillOpacity = createProp "fill-opacity"

fx ∷ ToStringAttribute
fx = createProp "fx"

fy ∷ ToStringAttribute
fy = createProp "fy"

markerHeight ∷ ToStringAttribute
markerHeight = createProp "markerHeight"

markerWidth ∷ ToStringAttribute
markerWidth = createProp "markerWidth"

r ∷ ToStringAttribute
r = createProp "r"

strokeDashoffset ∷ ToStringAttribute
strokeDashoffset = createProp "stroke-dashoffset"

strokeOpacity ∷ ToStringAttribute
strokeOpacity = createProp "stroke-opacity"

strokeWidth ∷ ToStringAttribute
strokeWidth = createProp "stroke-width"

textLength ∷ ToStringAttribute
textLength = createProp "textLength"

x ∷ ToStringAttribute
x = createProp "x"

x1 ∷ ToStringAttribute
x1 = createProp "x1"

x2 ∷ ToStringAttribute
x2 = createProp "x2"

y ∷ ToStringAttribute
y = createProp "y"

y1 ∷ ToStringAttribute
y1 = createProp "y1"

y2 ∷ ToStringAttribute
y2 = createProp "y2"

accumulate ∷ ToStringAttribute
accumulate = createProp "accumulate"

additive ∷ ToStringAttribute
additive = createProp "additive"

alignmentBaseline ∷ ToStringAttribute
alignmentBaseline = createProp "alignment-baseline"

baseFrequency ∷ ToStringAttribute
baseFrequency = createProp "baseFrequency"

baselineShift ∷ ToStringAttribute
baselineShift = createProp "baseline-shift"

baseProfile ∷ ToStringAttribute
baseProfile = createProp "baseProfile"

begin ∷ ToStringAttribute
begin = createProp "begin"

calcMode ∷ ToStringAttribute
calcMode = createProp "calcMode"

clipPathUnits ∷ ToStringAttribute
clipPathUnits = createProp "clipPathUnits"

clipPathAttr ∷ ToStringAttribute
clipPathAttr = createProp "clip-path"

clipRule ∷ ToStringAttribute
clipRule = createProp "clip-rule"

color ∷ ToStringAttribute
color = createProp "color"

colorInterpolation ∷ ToStringAttribute
colorInterpolation = createProp "color-interpolation"

colorInterpolationFilters ∷ ToStringAttribute
colorInterpolationFilters = createProp "color-interpolation-filters"

colorProfileAttr ∷ ToStringAttribute
colorProfileAttr = createProp "color-profile"

colorRendering ∷ ToStringAttribute
colorRendering = createProp "color-rendering"

contentScriptType ∷ ToStringAttribute
contentScriptType = createProp "contentScriptType"

contentStyleType ∷ ToStringAttribute
contentStyleType = createProp "contentStyleType"

cursorAttr ∷ ToStringAttribute
cursorAttr = createProp "cursor"

d ∷ ToStringAttribute
d = createProp "d"

direction ∷ ToStringAttribute
direction = createProp "direction"

display ∷ ToStringAttribute
display = createProp "display"

dominantBaseline ∷ ToStringAttribute
dominantBaseline = createProp "dominant-baseline"

dur ∷ ToStringAttribute
dur = createProp "dur"

dx ∷ ToStringAttribute
dx = createProp "dx"

dy ∷ ToStringAttribute
dy = createProp "dy"

edgeMode ∷ ToStringAttribute
edgeMode = createProp "edgeMode"

end ∷ ToStringAttribute
end = createProp "end"

fill ∷ ToStringAttribute
fill = createProp "fill"

fillRule ∷ ToStringAttribute
fillRule = createProp "fill-rule"

filterAttr ∷ ToStringAttribute
filterAttr = createProp "filter"

filterUnits ∷ ToStringAttribute
filterUnits = createProp "filterUnits"

floodColor ∷ ToStringAttribute
floodColor = createProp "flood-color"

floodOpacity ∷ ToStringAttribute
floodOpacity = createProp "flood-opacity"

fontFamily ∷ ToStringAttribute
fontFamily = createProp "font-family"

fontSize ∷ ToStringAttribute
fontSize = createProp "font-size"

fontSizeAdjust ∷ ToStringAttribute
fontSizeAdjust = createProp "font-size-adjust"

fontStretch ∷ ToStringAttribute
fontStretch = createProp "font-stretch"

fontStyle ∷ ToStringAttribute
fontStyle = createProp "font-style"

fontVariant ∷ ToStringAttribute
fontVariant = createProp "font-variant"

fontWeight ∷ ToStringAttribute
fontWeight = createProp "font-weight"

from ∷ ToStringAttribute
from = createProp "from"

gradientTransform ∷ ToStringAttribute
gradientTransform = createProp "gradientTransform"

gradientUnits ∷ ToStringAttribute
gradientUnits = createProp "gradientUnits"

imageRendering ∷ ToStringAttribute
imageRendering = createProp "image-rendering"

in' ∷ ToStringAttribute
in' = createProp "in"

in2 ∷ ToStringAttribute
in2 = createProp "in2"

kernelMatrix ∷ ToStringAttribute
kernelMatrix = createProp "kernelMatrix"

kernelUnitLength ∷ ToStringAttribute
kernelUnitLength = createProp "kernelUnitLength"

kerning ∷ ToStringAttribute
kerning = createProp "kerning"

keySplines ∷ ToStringAttribute
keySplines = createProp "keySplines"

keyTimes ∷ ToStringAttribute
keyTimes = createProp "keyTimes"

lengthAdjust ∷ ToStringAttribute
lengthAdjust = createProp "lengthAdjust"

letterSpacing ∷ ToStringAttribute
letterSpacing = createProp "letter-spacing"

lightingColor ∷ ToStringAttribute
lightingColor = createProp "lighting-color"

local ∷ ToStringAttribute
local = createProp "local"

markerEnd ∷ ToStringAttribute
markerEnd = createProp "marker-end"

markerMid ∷ ToStringAttribute
markerMid = createProp "marker-mid"

markerStart ∷ ToStringAttribute
markerStart = createProp "marker-start"

markerUnits ∷ ToStringAttribute
markerUnits = createProp "markerUnits"

maskAttr ∷ ToStringAttribute
maskAttr = createProp "mask"

maskContentUnits ∷ ToStringAttribute
maskContentUnits = createProp "maskContentUnits"

maskUnits ∷ ToStringAttribute
maskUnits = createProp "maskUnits"

mode ∷ ToStringAttribute
mode = createProp "mode"

opacity ∷ ToStringAttribute
opacity = createProp "opacity"

operator ∷ ToStringAttribute
operator = createProp "operator"

order ∷ ToStringAttribute
order = createProp "order"

overflow ∷ ToStringAttribute
overflow = createProp "overflow"

paintOrder ∷ ToStringAttribute
paintOrder = createProp "paint-order"

patternContentUnits ∷ ToStringAttribute
patternContentUnits = createProp "patternContentUnits"

patternTransform ∷ ToStringAttribute
patternTransform = createProp "patternTransform"

patternUnits ∷ ToStringAttribute
patternUnits = createProp "patternUnits"

pointerEvents ∷ ToStringAttribute
pointerEvents = createProp "pointer-events"

points ∷ ToStringAttribute
points = createProp "points"

preserveAspectRatio ∷ ToStringAttribute
preserveAspectRatio = createProp "preserveAspectRatio"

primitiveUnits ∷ ToStringAttribute
primitiveUnits = createProp "primitiveUnits"

radius ∷ ToStringAttribute
radius = createProp "radius"

repeatCount ∷ ToStringAttribute
repeatCount = createProp "repeatCount"

repeatDur ∷ ToStringAttribute
repeatDur = createProp "repeatDur"

requiredFeatures ∷ ToStringAttribute
requiredFeatures = createProp "requiredFeatures"

restart ∷ ToStringAttribute
restart = createProp "restart"

result ∷ ToStringAttribute
result = createProp "result"

rx ∷ ToStringAttribute
rx = createProp "rx"

ry ∷ ToStringAttribute
ry = createProp "ry"

shapeRendering ∷ ToStringAttribute
shapeRendering = createProp "shape-rendering"

stdDeviation ∷ ToStringAttribute
stdDeviation = createProp "stdDeviation"

stitchTiles ∷ ToStringAttribute
stitchTiles = createProp "stitchTiles"

stopColor ∷ ToStringAttribute
stopColor = createProp "stop-color"

stopOpacity ∷ ToStringAttribute
stopOpacity = createProp "stop-opacity"

stroke ∷ ToStringAttribute
stroke = createProp "stroke"

strokeDasharray ∷ ToStringAttribute
strokeDasharray = createProp "stroke-dasharray"

strokeLinecap ∷ ToStringAttribute
strokeLinecap = createProp "stroke-linecap"

strokeLinejoin ∷ ToStringAttribute
strokeLinejoin = createProp "stroke-linejoin"

styleAttr ∷ ToStringAttribute
styleAttr = createProp "style"

textAnchor ∷ ToStringAttribute
textAnchor = createProp "text-anchor"

textDecoration ∷ ToStringAttribute
textDecoration = createProp "text-decoration"

textRendering ∷ ToStringAttribute
textRendering = createProp "text-rendering"

transform ∷ ToStringAttribute
transform = createProp "transform"

values ∷ ToStringAttribute
values = createProp "values"

vectorEffect ∷ ToStringAttribute
vectorEffect = createProp "vector-effect"

viewBox ∷ ToStringAttribute
viewBox = createProp "viewBox"

visibility ∷ ToStringAttribute
visibility = createProp "visibility"

wordSpacing ∷ ToStringAttribute
wordSpacing = createProp "word-spacing"

writingMode ∷ ToStringAttribute
writingMode = createProp "writing-mode"

xChannelSelector ∷ ToStringAttribute
xChannelSelector = createProp "xChannelSelector"

yChannelSelector ∷ ToStringAttribute
yChannelSelector = createProp "yChannelSelector"

accentHeight ∷ ToNumberAttribute
accentHeight = createProp "accent-height" <<< show

ascent ∷ ToNumberAttribute
ascent = createProp "ascent" <<< show

azimuth ∷ ToNumberAttribute
azimuth = createProp "azimuth" <<< show

bias ∷ ToNumberAttribute
bias = createProp "bias" <<< show

diffuseConstant ∷ ToNumberAttribute
diffuseConstant = createProp "diffuseConstant" <<< show

divisor ∷ ToNumberAttribute
divisor = createProp "divisor" <<< show

elevation ∷ ToNumberAttribute
elevation = createProp "elevation" <<< show

fr ∷ ToNumberAttribute
fr = createProp "fr" <<< show

k1 ∷ ToNumberAttribute
k1 = createProp "k1" <<< show

k2 ∷ ToNumberAttribute
k2 = createProp "k2" <<< show

k3 ∷ ToNumberAttribute
k3 = createProp "k3" <<< show

k4 ∷ ToNumberAttribute
k4 = createProp "k4" <<< show

limitingConeAngle ∷ ToNumberAttribute
limitingConeAngle = createProp "limitingConeAngle" <<< show

overlinePosition ∷ ToNumberAttribute
overlinePosition = createProp "overline-position" <<< show

overlineThickness ∷ ToNumberAttribute
overlineThickness = createProp "overline-thickness" <<< show

pathLength ∷ ToNumberAttribute
pathLength = createProp "pathLength" <<< show

pointsAtX ∷ ToNumberAttribute
pointsAtX = createProp "pointsAtX" <<< show

pointsAtY ∷ ToNumberAttribute
pointsAtY = createProp "pointsAtY" <<< show

pointsAtZ ∷ ToNumberAttribute
pointsAtZ = createProp "pointsAtZ" <<< show

refX ∷ ToNumberAttribute
refX = createProp "refX" <<< show

refY ∷ ToNumberAttribute
refY = createProp "refY" <<< show

scale ∷ ToNumberAttribute
scale = createProp "scale" <<< show

seed ∷ ToNumberAttribute
seed = createProp "seed" <<< show

specularConstant ∷ ToNumberAttribute
specularConstant = createProp "specularConstant" <<< show

specularExponent ∷ ToNumberAttribute
specularExponent = createProp "specularExponent" <<< show

strikethroughPosition ∷ ToNumberAttribute
strikethroughPosition = createProp "strikethrough-position" <<< show

strikethroughThickness ∷ ToNumberAttribute
strikethroughThickness = createProp "strikethrough-thickness" <<< show

strokeMiterlimit ∷ ToNumberAttribute
strokeMiterlimit = createProp "stroke-miterlimit" <<< show

surfaceScale ∷ ToNumberAttribute
surfaceScale = createProp "surfaceScale" <<< show

targetX ∷ ToNumberAttribute
targetX = createProp "targetX" <<< show

targetY ∷ ToNumberAttribute
targetY = createProp "targetY" <<< show

underlinePosition ∷ ToNumberAttribute
underlinePosition = createProp "underline-position" <<< show

underlineThickness ∷ ToNumberAttribute
underlineThickness = createProp "underline-thickness" <<< show

version ∷ ToNumberAttribute
version = createProp "version" <<< show

numOctaves ∷ ToIntAttribute
numOctaves = createProp "numOctaves" <<< show

autocomplete ∷ ToStringAttribute
autocomplete = createProp "autocomplete"

autofocus ∷ ToBooleanAttribute
autofocus = createProp "autofocus" <<< booleanToFalsyString

autoplay ∷ ToBooleanAttribute
autoplay = createProp "autoplay" <<< booleanToFalsyString

checked ∷ ToBooleanAttribute
checked = createProp "checked" <<< booleanToFalsyString

contentEditable ∷ ToBooleanAttribute
contentEditable = createProp "contentEditable" <<< booleanToFalsyString

controls ∷ ToBooleanAttribute
controls = createProp "controls" <<< booleanToFalsyString

default ∷ ToBooleanAttribute
default = createProp "default" <<< booleanToFalsyString

disabled ∷ ToBooleanAttribute
disabled = createProp "disabled" <<< booleanToFalsyString

hidden ∷ ToBooleanAttribute
hidden = createProp "hidden" <<< booleanToFalsyString

isMap ∷ ToBooleanAttribute
isMap = createProp "isMap" <<< booleanToFalsyString

loop ∷ ToBooleanAttribute
loop = createProp "loop" <<< booleanToFalsyString

multiple ∷ ToBooleanAttribute
multiple = createProp "multiple" <<< booleanToFalsyString

noValidate ∷ ToBooleanAttribute
noValidate = createProp "noValidate" <<< booleanToFalsyString

readOnly ∷ ToBooleanAttribute
readOnly = createProp "readOnly" <<< booleanToFalsyString

required ∷ ToBooleanAttribute
required = createProp "required" <<< booleanToFalsyString

reversed ∷ ToBooleanAttribute
reversed = createProp "reversed" <<< booleanToFalsyString

selected ∷ ToBooleanAttribute
selected = createProp "selected" <<< booleanToFalsyString

spellcheck ∷ ToBooleanAttribute
spellcheck = createProp "spellcheck" <<< booleanToFalsyString

externalResourcesRequired ∷ ToBooleanAttribute
externalResourcesRequired = createProp "externalResourcesRequired" <<< booleanToFalsyString

preserveAlpha ∷ ToBooleanAttribute
preserveAlpha = createProp "preserveAlpha" <<< booleanToFalsyString
