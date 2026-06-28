module Flame.Application
      ( Update
      , App
      , Application
      , noMessages
      , mount
      , mount_
      ) where

import Data.Either (Either(..))
import Data.Foldable as DF
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Exception as EE
import Effect.Console as EC
import Effect.Ref as ER
import Flame.Renderer.Internal.Mobile as FRM
import Flame.Subscription.Internal.Listener as FSIL
import Flame.Types (AppId(..), ApplicationId, Html, RenderingState, Subscription)
import Prelude (class Show, Unit, bind, discard, pure, show, unit, ($), (<<<))
import Unsafe.Coerce as UC

type Update model message = model → message → Tuple model (Array (Aff (Maybe message)))

-- | Abstracts over common fields of an `Application`
type App model message extension =
      { view ∷ model → Html message
      , subscribe ∷ Array (Subscription message)
      | extension
      }

-- | `Application` contains
-- | * `model` – starting model
-- | * `view` – a function to update your markup
-- | * `update` – a function to update your model
-- | * `subscribe` – list of external events
type Application model message = App model message
      ( model ∷ model
      , update ∷ Update model message
      )

noMessages ∷ ∀ model message. model → Tuple model (Array (Aff (Maybe message)))
noMessages model = model /\ []


-- | Mount a Flame application that can be fed arbitrary external messages
mount ∷ ∀ id model message. Show id ⇒ AppId id message → Application model message → Effect Unit
mount (AppId id) = run (Just $ show id)

-- | Mount a Flame application
mount_ ∷ ∀ model message. Application model message → Effect Unit
mount_ = run Nothing

-- | Keeps the state in a `Ref` and call `Flame.Renderer.render` for every update
run ∷ ∀ model message. Maybe ApplicationId → Application model message → Effect Unit
run appId application = do
      renderingState ← ER.new (UC.unsafeCoerce 21 ∷ RenderingState)

      --the function which actually run events
      let
            runUpdate = case _ of
                  Nothing → pure unit
                  Just message → do
                        rendering ← ER.read renderingState
                        state <- FRM.getState rendering
                        let updatedState /\ affs = application.update state message
                        FRM.resume rendering updatedState
                        DF.for_ affs $ EA.runAff_
                              ( case _ of
                                      Left error → EC.log $ EE.message error
                                      Right msg → runUpdate msg
                              )


      rendering ← FRM.start runUpdate application.view application.model
      ER.write rendering renderingState

      --subscriptions are used for external events
      -- case appId of
      --       Nothing → pure unit
      --       Just id → FSIL.createMessageListener id runUpdate
      -- DF.traverse_ (FSIL.createSubscription (runUpdate <<< Just)) application.subscribe