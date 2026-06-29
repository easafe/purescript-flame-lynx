-- | Definition of HTML elements
module Flame.Html.Element where

import Prelude hiding (map)

import Data.Array as DA
import Data.Maybe (Maybe)
import Data.Maybe as DM
import Effect (Effect)
import Flame.Types (Html, Key, NodeData, Tag)
import Web.DOM (Node)
import Web.DOM.ParentNode (children)

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

foreign import createSvgChild ∷ ∀ message. Tag -> Array (NodeData message) → Array (Html message) → Html message

circle  :: forall message. Array (NodeData message) -> Array (Html message) -> Html message
circle nodeData children = createSvgChild "circle" nodeData children

clipPath :: forall message. Array (NodeData message) -> Array (Html message) -> Html message
clipPath nodeData children = createSvgChild "clipPath" nodeData children

defs :: forall message. Array (NodeData message) -> Array (Html message) -> Html message
defs nodeData children = createSvgChild "defs" nodeData children

ellipse :: forall message. Array (NodeData message) -> Array (Html message) -> Html message
ellipse nodeData children = createSvgChild "ellipse" nodeData children

g :: forall message. Array (NodeData message) -> Array (Html message) -> Html message
g nodeData children = createSvgChild "g" nodeData children

image :: forall message. Array (NodeData message) -> Array (Html message) -> Html message
image nodeData children = createSvgChild "image" nodeData children

line :: forall message. Array (NodeData message) -> Array (Html message) -> Html message
line nodeData children = createSvgChild "line" nodeData children

linearGradient :: forall message. Array (NodeData message) -> Array (Html message) -> Html message
linearGradient nodeData children = createSvgChild "linearGradient" nodeData children

path :: forall message. Array (NodeData message) -> Array (Html message) -> Html message
path nodeData children = createSvgChild "path" nodeData children

polygon :: forall message. Array (NodeData message) -> Array (Html message) -> Html message
polygon nodeData children = createSvgChild "polygon" nodeData children

polyline :: forall message. Array (NodeData message) -> Array (Html message) -> Html message
polyline nodeData children = createSvgChild "polyline" nodeData children

radialGradient :: forall message. Array (NodeData message) -> Array (Html message) -> Html message
radialGradient nodeData children = createSvgChild "radialGradient" nodeData children

rect :: forall message. Array (NodeData message) -> Array (Html message) -> Html message
rect nodeData children = createSvgChild "rect" nodeData children

stop :: forall message. Array (NodeData message) -> Array (Html message) -> Html message
stop nodeData children = createSvgChild "stop" nodeData children

use :: forall message. Array (NodeData message) -> Array (Html message) -> Html message
use nodeData children = createSvgChild "use" nodeData children

input ∷ ∀ message. Array (NodeData message) → Html message
input = createElement' "input"

input_ ∷ ∀ message. Array (Html message) → Html message
input_ = createElement_ "input"

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
