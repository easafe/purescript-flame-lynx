module Test.Subscription.Broadcast (mount, TSBMessage(..)) where

-- | Counter example using a side effects free function

import Prelude

import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Flame (Html)
import Flame as F
import Flame.Html.Attribute as FHA
import Flame.Html.Element as FHE
import Flame.Subscription as FS
import Flame.Types (AppId(..))
import Web.Event.Event (EventType(..))

-- | The model represents the state of the app
type Model = Int

-- | This datatype is used to signal events to `update`
data TSBMessage = TEELIncrement | TEELDecrement (Maybe Int)

-- | `update` is called to handle events
update ∷ Model → TSBMessage → Tuple Model (Array (Aff (Maybe TSBMessage)))
update model = case _ of
      TEELIncrement → (model + 1) /\ []
      TEELDecrement amount → (model - (DM.fromMaybe 1 amount)) /\ []

-- | `view` is called whenever the model is updated
view ∷ Model → Html TSBMessage
view model = FHE.main [FHA.id "main"]
      [ FHE.span [FHA.id "text-output"] [FHE.text $ show model]
      ]

-- | Mount the application on the given selector
mount ∷ Effect Unit
mount = do
      F.mount (AppId "broadcast")
            { model: 0
            , subscribe: [ FS.onCustomEvent (EventType "increment-event") TEELIncrement, FS.onCustomEvent' (EventType "decrement-event") (pure <<< Just <<< TEELDecrement) ]
            , update
            , view
            }