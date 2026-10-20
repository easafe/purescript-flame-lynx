-- | Defines helpers for events from outside the view (e.g., global events)
-- | For view events, see `Flame.Html.Attribute`
module Flame.Subscription
      ( send
      , onCustomEvent
      , onCustomEvent'
      ) where

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Flame.Subscription.Internal.GlobalEvent as FSIG
import Flame.Serialization (class UnserializeState)
import Flame.Serialization as FS
import Flame.Types (AppId, Subscription)
import Foreign as F
import Prelude (class Show, Unit, const, pure, show, (<<<))
import Web.Event.Event (EventType(..))

-- | Raises an arbitrary message on the given application
send ∷ ∀ id message. Show id ⇒ AppId id message → message → Effect Unit
send appId message = FSIG.dispatchCustomEvent (show appId) message

-- | Subscribe to a `CustomEvent` that has no data associated
onCustomEvent ∷ ∀ message. EventType → message → Subscription message
onCustomEvent (EventType eventName) message = eventName /\ pure <<< Just <<< const message

-- | Subscribe to a `CustomEvent`
-- |
-- | `arg` must be serializable since it might come from external JavaScript
onCustomEvent' ∷ ∀ arg message. UnserializeState arg ⇒ EventType → (arg → Effect (Maybe message)) → Subscription message
onCustomEvent' (EventType eventName) message = eventName /\ message <<< FS.unsafeUnserialize <<< F.unsafeFromForeign
