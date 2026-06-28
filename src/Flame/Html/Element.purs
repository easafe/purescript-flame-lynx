-- | Definition of HTML elements
module Flame.Html.Element where

import Prelude hiding (map)

import Data.Array as DA
import Data.Maybe (Maybe)
import Data.Maybe as DM
import Effect (Effect)
--import Flame.Internal.Fragment as FIF
import Flame.Types (Html, Key, NodeData, Tag)
import Web.DOM (Node)

-- | `NodeRenderer` contains
-- | * `createNode` – function to create a DOM node from the given data
-- | * `updateNode` – function to update a DOM node from previous and current data
type NodeRenderer arg =
      { createNode ∷ arg → Effect Node
      , updateNode ∷ Node → arg → arg → Effect Node
      }

-- foreign import createLazyNode ∷ ∀ message arg. Array String → (arg → Html message) → arg → Html message
-- foreign import createManagedNode ∷ ∀ arg message. NodeRenderer arg → Array (NodeData message) → arg → Html message
-- foreign import createDatalessManagedNode ∷ ∀ arg message. NodeRenderer arg → arg → Html message

-- | Creates a text node
foreign import text ∷ ∀ message. String → Html message

foreign import createElementNode ∷ ∀ message. Tag → Array (NodeData message) → Array (Html message) → Html message

-- | Creates an element node with attributes and children nodes
createElement ∷ ∀ message. Tag → Array (NodeData message) → Array (Html message) → Html message
createElement tag nodeData children = createElementNode tag nodeData children

foreign import createDatalessElementNode ∷ ∀ message. Tag → Array (Html message) → Html message

-- | Creates an element node with no attributes but children nodes
createElement_ ∷ ∀ message. Tag → Array (Html message) → Html message
createElement_ tag children = createDatalessElementNode tag children

foreign import createSingleElementNode ∷ ∀ message. Tag → Array (NodeData message) → Html message

-- | Creates an element node with attributes but no children nodes
createElement' ∷ ∀ message. Tag → Array (NodeData message) → Html message
createElement' tag nodeData = createSingleElementNode tag nodeData

-- -- | Creates a fragment node
-- -- |
-- -- | Fragments act as wrappers: only children nodes are rendered
-- fragment ∷ ∀ message. Array (Html message) → Html message
-- fragment children = FIF.createFragmentNode children

-- -- | Creates a lazy node
-- -- |
-- -- | Lazy nodes are only updated if the `arg` parameter changes (compared by reference)
-- lazy ∷ ∀ arg message. Maybe Key → (arg → Html message) → arg → Html message
-- lazy maybeKey render arg = createLazyNode (DM.maybe [] DA.singleton maybeKey) render arg

-- -- | Creates a node which the corresponding DOM node is created and updated from the given `arg`
-- managed ∷ ∀ arg message. NodeRenderer arg → Array (NodeData message) → arg → Html message
-- managed render nodeData arg = createManagedNode render nodeData arg

-- -- | Creates a node (with no attributes) which the corresponding DOM node is created and updated from the given `arg`
-- managed_ ∷ ∀ arg message. NodeRenderer arg → arg → Html message
-- managed_ render arg = createDatalessManagedNode render arg

--separate functions as svg are special babies
foreign import createSvgNode ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message

svg ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
svg nodeData children = createSvgNode nodeData children

input ∷ ∀ message. Array (NodeData message) → Html message
input = createElement' "input"

input_ ∷ ∀ message. Array (Html message) → Html message
input_ = createElement_ "input"

--script generated

b ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
b = createElement "text"

b_ ∷ ∀ message. Array (Html message) → Html message
b_ = createElement_ "text"

b' ∷ ∀ message. Array (NodeData message) → Html message
b' = createElement' "text"

blockquote ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
blockquote = createElement "text"

blockquote_ ∷ ∀ message. Array (Html message) → Html message
blockquote_ = createElement_ "text"

blockquote' ∷ ∀ message. Array (NodeData message) → Html message
blockquote' = createElement' "text"

body ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
body = createElement "view"

body_ ∷ ∀ message. Array (Html message) → Html message
body_ = createElement_ "view"

body' ∷ ∀ message. Array (NodeData message) → Html message
body' = createElement' "view"

button ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
button = createElement "view"

button_ ∷ ∀ message. Array (Html message) → Html message
button_ = createElement_ "view"

button' ∷ ∀ message. Array (NodeData message) → Html message
button' = createElement' "view"

div ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
div = createElement "view"

div_ ∷ ∀ message. Array (Html message) → Html message
div_ = createElement_ "view"

div' ∷ ∀ message. Array (NodeData message) → Html message
div' = createElement' "view"

h1 ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
h1 = createElement "text"

h1_ ∷ ∀ message. Array (Html message) → Html message
h1_ = createElement_ "text"

h1' ∷ ∀ message. Array (NodeData message) → Html message
h1' = createElement' "text"

h2 ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
h2 = createElement "text"

h2_ ∷ ∀ message. Array (Html message) → Html message
h2_ = createElement_ "text"

h2' ∷ ∀ message. Array (NodeData message) → Html message
h2' = createElement' "text"

h3 ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
h3 = createElement "text"

h3_ ∷ ∀ message. Array (Html message) → Html message
h3_ = createElement_ "text"

h3' ∷ ∀ message. Array (NodeData message) → Html message
h3' = createElement' "text"

head ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
head = createElement "view"

head_ ∷ ∀ message. Array (Html message) → Html message
head_ = createElement_ "view"

head' ∷ ∀ message. Array (NodeData message) → Html message
head' = createElement' "view"

html ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
html = createElement "view"

html_ ∷ ∀ message. Array (Html message) → Html message
html_ = createElement_ "view"

html' ∷ ∀ message. Array (NodeData message) → Html message
html' = createElement' "view"

i ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
i = createElement "text"

i_ ∷ ∀ message. Array (Html message) → Html message
i_ = createElement_ "text"

i' ∷ ∀ message. Array (NodeData message) → Html message
i' = createElement' "text"

label ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
label = createElement "view"

label_ ∷ ∀ message. Array (Html message) → Html message
label_ = createElement_ "view"

label' ∷ ∀ message. Array (NodeData message) → Html message
label' = createElement' "view"

li ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
li = createElement "list-item"

li_ ∷ ∀ message. Array (Html message) → Html message
li_ = createElement_ "list-item"

li' ∷ ∀ message. Array (NodeData message) → Html message
li' = createElement' "list-item"

main ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
main = createElement "view"

main_ ∷ ∀ message. Array (Html message) → Html message
main_ = createElement_ "view"

main' ∷ ∀ message. Array (NodeData message) → Html message
main' = createElement' "view"

ol ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
ol = createElement "list"

ol_ ∷ ∀ message. Array (Html message) → Html message
ol_ = createElement_ "list"

ol' ∷ ∀ message. Array (NodeData message) → Html message
ol' = createElement' "list"

img ∷ ∀ message. Array (NodeData message) → Html message
img = createElement' "image"

p ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
p = createElement "text"

p_ ∷ ∀ message. Array (Html message) → Html message
p_ = createElement_ "text"

p' ∷ ∀ message. Array (NodeData message) → Html message
p' = createElement' "text"

pre ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
pre = createElement "text"

pre_ ∷ ∀ message. Array (Html message) → Html message
pre_ = createElement_ "text"

pre' ∷ ∀ message. Array (NodeData message) → Html message
pre' = createElement' "text"

select ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
select = createElement "view"

select_ ∷ ∀ message. Array (Html message) → Html message
select_ = createElement_ "view"

select' ∷ ∀ message. Array (NodeData message) → Html message
select' = createElement' "view"

span ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
span = createElement "view"

span_ ∷ ∀ message. Array (Html message) → Html message
span_ = createElement_ "view"

span' ∷ ∀ message. Array (NodeData message) → Html message
span' = createElement' "view"

strong ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
strong = createElement "text"

strong_ ∷ ∀ message. Array (Html message) → Html message
strong_ = createElement_ "text"

strong' ∷ ∀ message. Array (NodeData message) → Html message
strong' = createElement' "text"

table ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
table = createElement "view"

table_ ∷ ∀ message. Array (Html message) → Html message
table_ = createElement_ "view"

table' ∷ ∀ message. Array (NodeData message) → Html message
table' = createElement' "view"

td ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
td = createElement "view"

td_ ∷ ∀ message. Array (Html message) → Html message
td_ = createElement_ "view"

td' ∷ ∀ message. Array (NodeData message) → Html message
td' = createElement' "view"

textarea ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
textarea = createElement "textarea"

textarea_ ∷ ∀ message. Array (Html message) → Html message
textarea_ = createElement_ "textarea"

textarea' ∷ ∀ message. Array (NodeData message) → Html message
textarea' = createElement' "textarea"

tr ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
tr = createElement "view"

tr_ ∷ ∀ message. Array (Html message) → Html message
tr_ = createElement_ "view"

tr' ∷ ∀ message. Array (NodeData message) → Html message
tr' = createElement' "view"

ul ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
ul = createElement "list"

ul_ ∷ ∀ message. Array (Html message) → Html message
ul_ = createElement_ "list"

ul' ∷ ∀ message. Array (NodeData message) → Html message
ul' = createElement' "list"

circle ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
circle = createElement "circle"

circle_ ∷ ∀ message. Array (Html message) → Html message
circle_ = createElement_ "circle"

circle' ∷ ∀ message. Array (NodeData message) → Html message
circle' = createElement' "circle"

clipPath ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
clipPath = createElement "clipPath"

clipPath_ ∷ ∀ message. Array (Html message) → Html message
clipPath_ = createElement_ "clipPath"

clipPath' ∷ ∀ message. Array (NodeData message) → Html message
clipPath' = createElement' "clipPath"

colorProfile ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
colorProfile = createElement "color-profile"

colorProfile_ ∷ ∀ message. Array (Html message) → Html message
colorProfile_ = createElement_ "color-profile"

colorProfile' ∷ ∀ message. Array (NodeData message) → Html message
colorProfile' = createElement' "color-profile"

cursor ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
cursor = createElement "cursor"

cursor_ ∷ ∀ message. Array (Html message) → Html message
cursor_ = createElement_ "cursor"

cursor' ∷ ∀ message. Array (NodeData message) → Html message
cursor' = createElement' "cursor"

defs ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
defs = createElement "defs"

defs_ ∷ ∀ message. Array (Html message) → Html message
defs_ = createElement_ "defs"

defs' ∷ ∀ message. Array (NodeData message) → Html message
defs' = createElement' "defs"

desc ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
desc = createElement "desc"

desc_ ∷ ∀ message. Array (Html message) → Html message
desc_ = createElement_ "desc"

desc' ∷ ∀ message. Array (NodeData message) → Html message
desc' = createElement' "desc"

discard ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
discard = createElement "discard"

discard_ ∷ ∀ message. Array (Html message) → Html message
discard_ = createElement_ "discard"

discard' ∷ ∀ message. Array (NodeData message) → Html message
discard' = createElement' "discard"

ellipse ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
ellipse = createElement "ellipse"

ellipse_ ∷ ∀ message. Array (Html message) → Html message
ellipse_ = createElement_ "ellipse"

ellipse' ∷ ∀ message. Array (NodeData message) → Html message
ellipse' = createElement' "ellipse"

feBlend ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feBlend = createElement "feBlend"

feBlend_ ∷ ∀ message. Array (Html message) → Html message
feBlend_ = createElement_ "feBlend"

feBlend' ∷ ∀ message. Array (NodeData message) → Html message
feBlend' = createElement' "feBlend"

feColorMatrix ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feColorMatrix = createElement "feColorMatrix"

feColorMatrix_ ∷ ∀ message. Array (Html message) → Html message
feColorMatrix_ = createElement_ "feColorMatrix"

feColorMatrix' ∷ ∀ message. Array (NodeData message) → Html message
feColorMatrix' = createElement' "feColorMatrix"

feComponentTransfer ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feComponentTransfer = createElement "feComponentTransfer"

feComponentTransfer_ ∷ ∀ message. Array (Html message) → Html message
feComponentTransfer_ = createElement_ "feComponentTransfer"

feComponentTransfer' ∷ ∀ message. Array (NodeData message) → Html message
feComponentTransfer' = createElement' "feComponentTransfer"

feComposite ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feComposite = createElement "feComposite"

feComposite_ ∷ ∀ message. Array (Html message) → Html message
feComposite_ = createElement_ "feComposite"

feComposite' ∷ ∀ message. Array (NodeData message) → Html message
feComposite' = createElement' "feComposite"

feConvolveMatrix ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feConvolveMatrix = createElement "feConvolveMatrix"

feConvolveMatrix_ ∷ ∀ message. Array (Html message) → Html message
feConvolveMatrix_ = createElement_ "feConvolveMatrix"

feConvolveMatrix' ∷ ∀ message. Array (NodeData message) → Html message
feConvolveMatrix' = createElement' "feConvolveMatrix"

feDiffuseLighting ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feDiffuseLighting = createElement "feDiffuseLighting"

feDiffuseLighting_ ∷ ∀ message. Array (Html message) → Html message
feDiffuseLighting_ = createElement_ "feDiffuseLighting"

feDiffuseLighting' ∷ ∀ message. Array (NodeData message) → Html message
feDiffuseLighting' = createElement' "feDiffuseLighting"

feDisplacementMap ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feDisplacementMap = createElement "feDisplacementMap"

feDisplacementMap_ ∷ ∀ message. Array (Html message) → Html message
feDisplacementMap_ = createElement_ "feDisplacementMap"

feDisplacementMap' ∷ ∀ message. Array (NodeData message) → Html message
feDisplacementMap' = createElement' "feDisplacementMap"

feDistantLight ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feDistantLight = createElement "feDistantLight"

feDistantLight_ ∷ ∀ message. Array (Html message) → Html message
feDistantLight_ = createElement_ "feDistantLight"

feDistantLight' ∷ ∀ message. Array (NodeData message) → Html message
feDistantLight' = createElement' "feDistantLight"

feDropShadow ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feDropShadow = createElement "feDropShadow"

feDropShadow_ ∷ ∀ message. Array (Html message) → Html message
feDropShadow_ = createElement_ "feDropShadow"

feDropShadow' ∷ ∀ message. Array (NodeData message) → Html message
feDropShadow' = createElement' "feDropShadow"

feFlood ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feFlood = createElement "feFlood"

feFlood_ ∷ ∀ message. Array (Html message) → Html message
feFlood_ = createElement_ "feFlood"

feFlood' ∷ ∀ message. Array (NodeData message) → Html message
feFlood' = createElement' "feFlood"

feFuncA ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feFuncA = createElement "feFuncA"

feFuncA_ ∷ ∀ message. Array (Html message) → Html message
feFuncA_ = createElement_ "feFuncA"

feFuncA' ∷ ∀ message. Array (NodeData message) → Html message
feFuncA' = createElement' "feFuncA"

feFuncB ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feFuncB = createElement "feFuncB"

feFuncB_ ∷ ∀ message. Array (Html message) → Html message
feFuncB_ = createElement_ "feFuncB"

feFuncB' ∷ ∀ message. Array (NodeData message) → Html message
feFuncB' = createElement' "feFuncB"

feFuncG ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feFuncG = createElement "feFuncG"

feFuncG_ ∷ ∀ message. Array (Html message) → Html message
feFuncG_ = createElement_ "feFuncG"

feFuncG' ∷ ∀ message. Array (NodeData message) → Html message
feFuncG' = createElement' "feFuncG"

feFuncR ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feFuncR = createElement "feFuncR"

feFuncR_ ∷ ∀ message. Array (Html message) → Html message
feFuncR_ = createElement_ "feFuncR"

feFuncR' ∷ ∀ message. Array (NodeData message) → Html message
feFuncR' = createElement' "feFuncR"

feGaussianBlur ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feGaussianBlur = createElement "feGaussianBlur"

feGaussianBlur_ ∷ ∀ message. Array (Html message) → Html message
feGaussianBlur_ = createElement_ "feGaussianBlur"

feGaussianBlur' ∷ ∀ message. Array (NodeData message) → Html message
feGaussianBlur' = createElement' "feGaussianBlur"

feImage ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feImage = createElement "feImage"

feImage_ ∷ ∀ message. Array (Html message) → Html message
feImage_ = createElement_ "feImage"

feImage' ∷ ∀ message. Array (NodeData message) → Html message
feImage' = createElement' "feImage"

feMerge ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feMerge = createElement "feMerge"

feMerge_ ∷ ∀ message. Array (Html message) → Html message
feMerge_ = createElement_ "feMerge"

feMerge' ∷ ∀ message. Array (NodeData message) → Html message
feMerge' = createElement' "feMerge"

feMergeNode ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feMergeNode = createElement "feMergeNode"

feMergeNode_ ∷ ∀ message. Array (Html message) → Html message
feMergeNode_ = createElement_ "feMergeNode"

feMergeNode' ∷ ∀ message. Array (NodeData message) → Html message
feMergeNode' = createElement' "feMergeNode"

feMorphology ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feMorphology = createElement "feMorphology"

feMorphology_ ∷ ∀ message. Array (Html message) → Html message
feMorphology_ = createElement_ "feMorphology"

feMorphology' ∷ ∀ message. Array (NodeData message) → Html message
feMorphology' = createElement' "feMorphology"

feOffset ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feOffset = createElement "feOffset"

feOffset_ ∷ ∀ message. Array (Html message) → Html message
feOffset_ = createElement_ "feOffset"

feOffset' ∷ ∀ message. Array (NodeData message) → Html message
feOffset' = createElement' "feOffset"

fePointLight ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
fePointLight = createElement "fePointLight"

fePointLight_ ∷ ∀ message. Array (Html message) → Html message
fePointLight_ = createElement_ "fePointLight"

fePointLight' ∷ ∀ message. Array (NodeData message) → Html message
fePointLight' = createElement' "fePointLight"

feSpecularLighting ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feSpecularLighting = createElement "feSpecularLighting"

feSpecularLighting_ ∷ ∀ message. Array (Html message) → Html message
feSpecularLighting_ = createElement_ "feSpecularLighting"

feSpecularLighting' ∷ ∀ message. Array (NodeData message) → Html message
feSpecularLighting' = createElement' "feSpecularLighting"

feSpotLight ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feSpotLight = createElement "feSpotLight"

feSpotLight_ ∷ ∀ message. Array (Html message) → Html message
feSpotLight_ = createElement_ "feSpotLight"

feSpotLight' ∷ ∀ message. Array (NodeData message) → Html message
feSpotLight' = createElement' "feSpotLight"

feTile ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feTile = createElement "feTile"

feTile_ ∷ ∀ message. Array (Html message) → Html message
feTile_ = createElement_ "feTile"

feTile' ∷ ∀ message. Array (NodeData message) → Html message
feTile' = createElement' "feTile"

feTurbulence ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
feTurbulence = createElement "feTurbulence"

feTurbulence_ ∷ ∀ message. Array (Html message) → Html message
feTurbulence_ = createElement_ "feTurbulence"

feTurbulence' ∷ ∀ message. Array (NodeData message) → Html message
feTurbulence' = createElement' "feTurbulence"

filter ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
filter = createElement "filter"

filter_ ∷ ∀ message. Array (Html message) → Html message
filter_ = createElement_ "filter"

filter' ∷ ∀ message. Array (NodeData message) → Html message
filter' = createElement' "filter"

font ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
font = createElement "font"

font_ ∷ ∀ message. Array (Html message) → Html message
font_ = createElement_ "font"

font' ∷ ∀ message. Array (NodeData message) → Html message
font' = createElement' "font"

fontFace ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
fontFace = createElement "font-face"

fontFace_ ∷ ∀ message. Array (Html message) → Html message
fontFace_ = createElement_ "font-face"

fontFace' ∷ ∀ message. Array (NodeData message) → Html message
fontFace' = createElement' "font-face"

fontFaceFormat ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
fontFaceFormat = createElement "font-face-format"

fontFaceFormat_ ∷ ∀ message. Array (Html message) → Html message
fontFaceFormat_ = createElement_ "font-face-format"

fontFaceFormat' ∷ ∀ message. Array (NodeData message) → Html message
fontFaceFormat' = createElement' "font-face-format"

fontFaceName ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
fontFaceName = createElement "font-face-name"

fontFaceName_ ∷ ∀ message. Array (Html message) → Html message
fontFaceName_ = createElement_ "font-face-name"

fontFaceName' ∷ ∀ message. Array (NodeData message) → Html message
fontFaceName' = createElement' "font-face-name"

fontFaceSrc ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
fontFaceSrc = createElement "font-face-src"

fontFaceSrc_ ∷ ∀ message. Array (Html message) → Html message
fontFaceSrc_ = createElement_ "font-face-src"

fontFaceSrc' ∷ ∀ message. Array (NodeData message) → Html message
fontFaceSrc' = createElement' "font-face-src"

fontFaceUri ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
fontFaceUri = createElement "font-face-uri"

fontFaceUri_ ∷ ∀ message. Array (Html message) → Html message
fontFaceUri_ = createElement_ "font-face-uri"

fontFaceUri' ∷ ∀ message. Array (NodeData message) → Html message
fontFaceUri' = createElement' "font-face-uri"

foreignObject ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
foreignObject = createElement "foreignObject"

foreignObject_ ∷ ∀ message. Array (Html message) → Html message
foreignObject_ = createElement_ "foreignObject"

foreignObject' ∷ ∀ message. Array (NodeData message) → Html message
foreignObject' = createElement' "foreignObject"

g ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
g = createElement "g"

g_ ∷ ∀ message. Array (Html message) → Html message
g_ = createElement_ "g"

g' ∷ ∀ message. Array (NodeData message) → Html message
g' = createElement' "g"

glyph ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
glyph = createElement "glyph"

glyph_ ∷ ∀ message. Array (Html message) → Html message
glyph_ = createElement_ "glyph"

glyph' ∷ ∀ message. Array (NodeData message) → Html message
glyph' = createElement' "glyph"

glyphRef ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
glyphRef = createElement "glyphRef"

glyphRef_ ∷ ∀ message. Array (Html message) → Html message
glyphRef_ = createElement_ "glyphRef"

glyphRef' ∷ ∀ message. Array (NodeData message) → Html message
glyphRef' = createElement' "glyphRef"

hatch ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
hatch = createElement "hatch"

hatch_ ∷ ∀ message. Array (Html message) → Html message
hatch_ = createElement_ "hatch"

hatch' ∷ ∀ message. Array (NodeData message) → Html message
hatch' = createElement' "hatch"

line ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
line = createElement "line"

line_ ∷ ∀ message. Array (Html message) → Html message
line_ = createElement_ "line"

line' ∷ ∀ message. Array (NodeData message) → Html message
line' = createElement' "line"

linearGradient ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
linearGradient = createElement "linearGradient"

linearGradient_ ∷ ∀ message. Array (Html message) → Html message
linearGradient_ = createElement_ "linearGradient"

linearGradient' ∷ ∀ message. Array (NodeData message) → Html message
linearGradient' = createElement' "linearGradient"

marker ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
marker = createElement "marker"

marker_ ∷ ∀ message. Array (Html message) → Html message
marker_ = createElement_ "marker"

marker' ∷ ∀ message. Array (NodeData message) → Html message
marker' = createElement' "marker"

mask ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
mask = createElement "mask"

mask_ ∷ ∀ message. Array (Html message) → Html message
mask_ = createElement_ "mask"

mask' ∷ ∀ message. Array (NodeData message) → Html message
mask' = createElement' "mask"

mesh ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
mesh = createElement "mesh"

mesh_ ∷ ∀ message. Array (Html message) → Html message
mesh_ = createElement_ "mesh"

mesh' ∷ ∀ message. Array (NodeData message) → Html message
mesh' = createElement' "mesh"

meshgradient ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
meshgradient = createElement "meshgradient"

meshgradient_ ∷ ∀ message. Array (Html message) → Html message
meshgradient_ = createElement_ "meshgradient"

meshgradient' ∷ ∀ message. Array (NodeData message) → Html message
meshgradient' = createElement' "meshgradient"

meshpatch ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
meshpatch = createElement "meshpatch"

meshpatch_ ∷ ∀ message. Array (Html message) → Html message
meshpatch_ = createElement_ "meshpatch"

meshpatch' ∷ ∀ message. Array (NodeData message) → Html message
meshpatch' = createElement' "meshpatch"

meshrow ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
meshrow = createElement "meshrow"

meshrow_ ∷ ∀ message. Array (Html message) → Html message
meshrow_ = createElement_ "meshrow"

meshrow' ∷ ∀ message. Array (NodeData message) → Html message
meshrow' = createElement' "meshrow"

metadata ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
metadata = createElement "metadata"

metadata_ ∷ ∀ message. Array (Html message) → Html message
metadata_ = createElement_ "metadata"

metadata' ∷ ∀ message. Array (NodeData message) → Html message
metadata' = createElement' "metadata"

missingGlyph ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
missingGlyph = createElement "missing-glyph"

missingGlyph_ ∷ ∀ message. Array (Html message) → Html message
missingGlyph_ = createElement_ "missing-glyph"

missingGlyph' ∷ ∀ message. Array (NodeData message) → Html message
missingGlyph' = createElement' "missing-glyph"

mpath ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
mpath = createElement "mpath"

mpath_ ∷ ∀ message. Array (Html message) → Html message
mpath_ = createElement_ "mpath"

mpath' ∷ ∀ message. Array (NodeData message) → Html message
mpath' = createElement' "mpath"

path ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
path = createElement "path"

path_ ∷ ∀ message. Array (Html message) → Html message
path_ = createElement_ "path"

path' ∷ ∀ message. Array (NodeData message) → Html message
path' = createElement' "path"

pattern ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
pattern = createElement "pattern"

pattern_ ∷ ∀ message. Array (Html message) → Html message
pattern_ = createElement_ "pattern"

pattern' ∷ ∀ message. Array (NodeData message) → Html message
pattern' = createElement' "pattern"

polygon ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
polygon = createElement "polygon"

polygon_ ∷ ∀ message. Array (Html message) → Html message
polygon_ = createElement_ "polygon"

polygon' ∷ ∀ message. Array (NodeData message) → Html message
polygon' = createElement' "polygon"

polyline ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
polyline = createElement "polyline"

polyline_ ∷ ∀ message. Array (Html message) → Html message
polyline_ = createElement_ "polyline"

polyline' ∷ ∀ message. Array (NodeData message) → Html message
polyline' = createElement' "polyline"

radialGradient ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
radialGradient = createElement "radialGradient"

radialGradient_ ∷ ∀ message. Array (Html message) → Html message
radialGradient_ = createElement_ "radialGradient"

radialGradient' ∷ ∀ message. Array (NodeData message) → Html message
radialGradient' = createElement' "radialGradient"

rect ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
rect = createElement "rect"

rect_ ∷ ∀ message. Array (Html message) → Html message
rect_ = createElement_ "rect"

rect' ∷ ∀ message. Array (NodeData message) → Html message
rect' = createElement' "rect"

script ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
script = createElement "script"

script_ ∷ ∀ message. Array (Html message) → Html message
script_ = createElement_ "script"

script' ∷ ∀ message. Array (NodeData message) → Html message
script' = createElement' "script"

set ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
set = createElement "set"

set_ ∷ ∀ message. Array (Html message) → Html message
set_ = createElement_ "set"

set' ∷ ∀ message. Array (NodeData message) → Html message
set' = createElement' "set"

solidcolor ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
solidcolor = createElement "solidcolor"

solidcolor_ ∷ ∀ message. Array (Html message) → Html message
solidcolor_ = createElement_ "solidcolor"

solidcolor' ∷ ∀ message. Array (NodeData message) → Html message
solidcolor' = createElement' "solidcolor"

stop ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
stop = createElement "stop"

stop_ ∷ ∀ message. Array (Html message) → Html message
stop_ = createElement_ "stop"

stop' ∷ ∀ message. Array (NodeData message) → Html message
stop' = createElement' "stop"

switch ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
switch = createElement "switch"

switch_ ∷ ∀ message. Array (Html message) → Html message
switch_ = createElement_ "switch"

switch' ∷ ∀ message. Array (NodeData message) → Html message
switch' = createElement' "switch"

symbol ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
symbol = createElement "symbol"

symbol_ ∷ ∀ message. Array (Html message) → Html message
symbol_ = createElement_ "symbol"

symbol' ∷ ∀ message. Array (NodeData message) → Html message
symbol' = createElement' "symbol"

textPath ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
textPath = createElement "textPath"

textPath_ ∷ ∀ message. Array (Html message) → Html message
textPath_ = createElement_ "textPath"

textPath' ∷ ∀ message. Array (NodeData message) → Html message
textPath' = createElement' "textPath"

tref ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
tref = createElement "tref"

tref_ ∷ ∀ message. Array (Html message) → Html message
tref_ = createElement_ "tref"

tref' ∷ ∀ message. Array (NodeData message) → Html message
tref' = createElement' "tref"

tspan ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
tspan = createElement "tspan"

tspan_ ∷ ∀ message. Array (Html message) → Html message
tspan_ = createElement_ "tspan"

tspan' ∷ ∀ message. Array (NodeData message) → Html message
tspan' = createElement' "tspan"

unknown ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
unknown = createElement "unknown"

unknown_ ∷ ∀ message. Array (Html message) → Html message
unknown_ = createElement_ "unknown"

unknown' ∷ ∀ message. Array (NodeData message) → Html message
unknown' = createElement' "unknown"

use ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
use = createElement "use"

use_ ∷ ∀ message. Array (Html message) → Html message
use_ = createElement_ "use"

use' ∷ ∀ message. Array (NodeData message) → Html message
use' = createElement' "use"

view ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
view = createElement "view"

view_ ∷ ∀ message. Array (Html message) → Html message
view_ = createElement_ "view"

view' ∷ ∀ message. Array (NodeData message) → Html message
view' = createElement' "view"

