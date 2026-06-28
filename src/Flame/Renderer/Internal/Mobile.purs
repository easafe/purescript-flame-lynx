module Flame.Renderer.Internal.Mobile
      ( start
      , resume
      , getState
      ) where

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3)
import Effect.Uncurried as EU
import Flame.Types (DomNode, RenderingState, Html)
import Prelude (Unit)

foreign import start_ ∷ ∀ model message. EffectFn3 (Maybe message → Effect Unit) (model -> Html message) model RenderingState

-- | Mounts the application
start ∷ ∀ model message. (Maybe message → Effect Unit) → (model -> Html message) → model -> Effect RenderingState
start = EU.runEffectFn3 start_

foreign import resume_ ∷ ∀ model. EffectFn2 RenderingState model Unit

resume ∷ ∀ model. RenderingState → model → Effect Unit
resume = EU.runEffectFn2 resume_

foreign import getState_ ∷ ∀ model. EffectFn1 RenderingState model

getState :: ∀ model. RenderingState → Effect model
getState = EU.runEffectFn1 getState_