module Flame.Subscription.Internal.GlobalEvent
      ( createCustomListener
      , dispatchCustomEvent
      ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2)
import Effect.Uncurried as EU
import Flame.Types (EventName)
import Foreign (Foreign)

foreign import createCustomListener_ ∷ EffectFn2 EventName (Foreign → Effect Unit) Unit
foreign import dispatchCustomEvent_ ∷ ∀ message. EffectFn2 EventName message Unit

createCustomListener ∷ EventName → (Foreign → Effect Unit) → Effect Unit
createCustomListener = EU.runEffectFn2 createCustomListener_

dispatchCustomEvent ∷ ∀ message. EventName → message → Effect Unit
dispatchCustomEvent = EU.runEffectFn2 dispatchCustomEvent_
