module Flame.Subscription.Internal.Listener
      ( createMessageListener
      , createSubscription
      ) where

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Flame.Subscription.Internal.GlobalEvent as FSIG
import Flame.Types (ApplicationId, Subscription)
import Foreign (Foreign)
import Foreign as F
import Prelude (Unit, bind, discard, pure, unit, (<<<))

foreign import checkApplicationId_ ∷ EffectFn1 String Unit

-- | Raises an error if application id is not unique
checkApplicationId ∷ String → Effect Unit
checkApplicationId = EU.runEffectFn1 checkApplicationId_

-- | Listener for external messages
-- |
-- | Implemented as a custom event because messages can be raised from anywhere
createMessageListener ∷ ∀ message. ApplicationId → (message → Effect Unit) → Effect Unit
createMessageListener appId updater = do
      checkApplicationId appId
      FSIG.createCustomListener appId (updater <<< F.unsafeFromForeign)

-- | Events from `Application.subscribe`
createSubscription ∷ ∀ message. (Effect Unit → Effect Unit) → (message → Effect Unit) → Subscription message → Effect Unit
createSubscription schedule updater (eventName /\ toMessage) = FSIG.createCustomListener eventName (schedule <<< run)
      where
      run ∷ Foreign → Effect Unit
      run arg = do
            message ← toMessage arg
            case message of
                  Just m → updater m
                  Nothing → pure unit
