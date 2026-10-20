## Flame ![build status](https://github.com/easafe/purescript-flame/actions/workflows/CI.yml/badge.svg)

This package provides (almost the) same API as the DOM based Flame library but for LynxJS. Import it for your mobile builds and voila, shared codebase

### Documentation

See the [project page](https://flame.asafe.dev/) or [pursuit](https://pursuit.purescript.org/packages/purescript-flame)

### Quick start

Install:

```bash
spago install flame-lynx
```

Example counter app:

```purescript
module Counter.Main where

import Prelude

import Effect (Effect)
import Flame (AppId(..), Html, Subscription, Update)
import Flame as F
import Flame.Html.Attribute as FHA
import Flame.Html.Element as FHE

-- | The model represents the state of the app
type Model = Int

-- | Data type used to represent events
data Message = Increment | Decrement

-- | Initial state of the app
init ∷ Model
init = 0

-- | Update the model without scheduling any effects
update ∷ Update Model Message
update model = case _ of
      Increment → F.noMessages $ model + 1
      Decrement → F.noMessages $ model - 1

-- | Render Lynx views and text, with onClick handling tap events
view ∷ Model → Html Message
view model = FHE.createElement "view" [ FHA.id "main" ]
      [ FHE.createElement "view" [ FHA.onClick Decrement ] [ FHE.text "-" ]
      , FHE.text $ show model
      , FHE.createElement "view" [ FHA.onClick Increment ] [ FHE.text "+" ]
      ]

-- | Events that come from outside the view
subscribe ∷ Array (Subscription Message)
subscribe = []

-- | Mount the application in the Lynx root with a unique application ID
main ∷ Effect Unit
main = F.mount (AppId "counter")
      { model: init
      , view
      , update
      , subscribe
      }
```

### Tools

[breeze](https://github.com/easafe/haskell-breeze) can be used to generate Flame markup from HTML

### Licensing

Licenses for loadash, stage0 and snabbdom-to-html added under [licenses](licenses/) since parts of the rendering code was adapted from these projects

### Funding

If this project is useful for you, consider [throwing a buck](https://asafe.dev/donate) to keep development possible
