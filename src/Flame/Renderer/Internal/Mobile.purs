module Flame.Renderer.Internal.Mobile
      ( create
      , navigate
      , start
      , resume
      , getState
      , enqueue
      ) where

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn5)
import Effect.Uncurried as EU
import Flame.Types (RenderingState, Html)
import Prelude (Unit, map)

foreign import create_ ∷ ∀ model message. EffectFn5 String (RenderingState → Maybe message → Effect Unit) (model → Html message) model (Effect (Nullable String)) RenderingState

create ∷ ∀ model message. String → (RenderingState → Maybe message → Effect Unit) → (model → Html message) → model → Effect (Maybe String) → Effect RenderingState
create appId updater view model check = EU.runEffectFn5 create_ appId updater view model (map toNullable check)

foreign import start_ ∷ EffectFn1 (Array RenderingState) Unit

start ∷ Array RenderingState → Effect Unit
start = EU.runEffectFn1 start_

foreign import resume_ ∷ ∀ model. EffectFn2 RenderingState model Unit

resume ∷ ∀ model. RenderingState → model → Effect Unit
resume = EU.runEffectFn2 resume_

foreign import getState_ ∷ ∀ model. EffectFn1 RenderingState model

getState :: ∀ model. RenderingState → Effect model
getState = EU.runEffectFn1 getState_

foreign import enqueue_ ∷ EffectFn2 RenderingState (Effect Unit) Unit

enqueue ∷ RenderingState → Effect Unit → Effect Unit
enqueue = EU.runEffectFn2 enqueue_

foreign import navigate_ ∷ EffectFn1 String Unit

navigate ∷ String → Effect Unit
navigate = EU.runEffectFn1 navigate_
