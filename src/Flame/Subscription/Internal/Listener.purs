module Flame.Subscription.Internal.Listener
      ( checkApplicationId
      , isApplicationRegistered
      , createMessageListener
      , createSubscription
      ) where

import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Flame.Subscription.Internal.GlobalEvent as FSIG
import Flame.Types (ApplicationId, Subscription)
import Foreign as F
import Prelude (Unit, discard, (<<<), (>>=))

foreign import checkApplicationId_ ∷ EffectFn1 String Unit
foreign import registerApplicationId_ ∷ EffectFn1 ApplicationId Unit
foreign import isApplicationRegistered_ ∷ EffectFn1 ApplicationId Boolean

-- | Raises an error if application id is not unique
checkApplicationId ∷ String → Effect Unit
checkApplicationId = EU.runEffectFn1 checkApplicationId_

isApplicationRegistered ∷ ApplicationId → Effect Boolean
isApplicationRegistered = EU.runEffectFn1 isApplicationRegistered_

-- | Listener for external messages
-- |
-- | Implemented as a custom event because messages can be raised from anywhere
createMessageListener ∷ ∀ message. ApplicationId → (message → Effect Unit) → Effect Unit
createMessageListener appId updater = do
      FSIG.createCustomListener appId (updater <<< F.unsafeFromForeign)
      EU.runEffectFn1 registerApplicationId_ appId

-- | Events from `Application.subscribe`
createSubscription ∷ ∀ message. (Maybe message → Effect Unit) → Subscription message → Effect Unit
createSubscription updater (eventName /\ toMessage) = FSIG.createCustomListener eventName \event → toMessage event >>= updater
