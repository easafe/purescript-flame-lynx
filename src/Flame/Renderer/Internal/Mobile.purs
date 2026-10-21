module Flame.Renderer.Internal.Mobile
      ( create
      , Initialization
      , navigate
      , start
      , resume
      , getState
      ) where

import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Console as EC
import Effect.Exception as EE
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3)
import Effect.Uncurried as EU
import Flame.Types (RenderingState, Html)
import Prelude (Unit, discard, map, ($))

type Initialization model message =
      { model ∷ model
      , view ∷ model → Html message
      , update ∷ Maybe message → Effect Unit
      , subscribe ∷ Effect Unit
      }

foreign import create_ ∷ ∀ model message. EffectFn3 String (RenderingState → Effect Unit → (Initialization model message → Effect Unit) → Effect Unit) (Effect (Nullable String)) RenderingState

create ∷ ∀ model message. String → (RenderingState → Aff (Initialization model message)) → Effect (Maybe String) → Effect RenderingState
create appId factory check = EU.runEffectFn3 create_ appId initialize (map toNullable check)
      where
      initialize rendering failed ready = EA.runAff_
            ( case _ of
                    Left error → do
                          EC.log $ EE.message error
                          failed
                    Right application → do
                          ready application
                          application.subscribe
            )
            (factory rendering)

foreign import start_ ∷ EffectFn1 (Array RenderingState) Unit

start ∷ Array RenderingState → Effect Unit
start = EU.runEffectFn1 start_

foreign import resume_ ∷ ∀ model. EffectFn2 RenderingState model Unit

resume ∷ ∀ model. RenderingState → model → Effect Unit
resume = EU.runEffectFn2 resume_

foreign import getState_ ∷ ∀ model. EffectFn1 RenderingState model

getState :: ∀ model. RenderingState → Effect model
getState = EU.runEffectFn1 getState_

foreign import navigate_ ∷ EffectFn1 String Unit

navigate ∷ String → Effect Unit
navigate = EU.runEffectFn1 navigate_
