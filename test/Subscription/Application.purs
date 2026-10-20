module Test.Subscription.EffectList (mount, TEELMessage(..)) where

-- | Counter example using a side effects free function

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Flame (Html)
import Flame as F
import Flame.Html.Attribute as FHA
import Flame.Html.Element as FHE
import Flame.Types (AppId(..))

-- | The model represents the state of the app
type Model = Int

-- | This datatype is used to signal events to `update`
data TEELMessage = TEELIncrement | TEELDecrement

-- | `update` is called to handle events
update ∷ Model → TEELMessage → Tuple Model (Array (Aff (Maybe TEELMessage)))
update model = case _ of
      TEELIncrement → (model + 1) /\ []
      TEELDecrement → (model - 1) /\ []

-- | `view` is called whenever the model is updated
view ∷ Model → Html TEELMessage
view model = FHE.main [FHA.id "main"]
      [ FHE.span [FHA.id "text-output"] [ FHE.text $ show model]
      ]

-- | Mount the application on the given selector
mount ∷ Effect (AppId String TEELMessage)
mount = do
      let id = AppId "teel"
      F.mount id
            { model: 0
            , subscribe: []
            , update
            , view
            }
      pure id