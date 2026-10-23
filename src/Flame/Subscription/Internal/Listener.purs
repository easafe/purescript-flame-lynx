module Flame.Subscription.Internal.Listener
      ( checkApplicationId
      , queueMessage
      , isApplicationInitialized
      , createMessageListener
      , createSubscription
      ) where

import Data.Foldable as DF
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2)
import Effect.Uncurried as EU
import Flame.Subscription.Internal.GlobalEvent as FSIG
import Flame.Types (ApplicationId, Subscription)
import Foreign (Foreign)
import Foreign as F
import Prelude (Unit, bind, discard, (<<<), (>>=))

foreign import checkApplicationId_ ∷ EffectFn1 String Unit
foreign import registerApplicationId_ ∷ EffectFn1 ApplicationId (Array Foreign)
foreign import queueMessage_ ∷ ∀ message. EffectFn2 ApplicationId message Unit
foreign import isApplicationInitialized_ ∷ EffectFn1 ApplicationId Boolean

-- | Raises an error if application id is not unique
checkApplicationId ∷ String → Effect Unit
checkApplicationId = EU.runEffectFn1 checkApplicationId_

isApplicationInitialized ∷ ApplicationId → Effect Boolean
isApplicationInitialized = EU.runEffectFn1 isApplicationInitialized_

queueMessage ∷ ∀ message. ApplicationId → message → Effect Unit
queueMessage = EU.runEffectFn2 queueMessage_

-- | Listener for external messages
-- |
-- | Implemented as a custom event because messages can be raised from anywhere
createMessageListener ∷ ∀ message. ApplicationId → (message → Effect Unit) → Effect Unit
createMessageListener appId updater = do
      FSIG.createCustomListener appId (updater <<< F.unsafeFromForeign)
      messages ← EU.runEffectFn1 registerApplicationId_ appId
      DF.traverse_ (updater <<< F.unsafeFromForeign) messages

-- | Events from `Application.subscribe`
createSubscription ∷ ∀ message. (Maybe message → Effect Unit) → Subscription message → Effect Unit
createSubscription updater (eventName /\ toMessage) = FSIG.createCustomListener eventName \event → toMessage event >>= updater
