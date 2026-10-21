module Flame.Application
      ( Update
      , Application
      , Route
      , makeRoute
      , makeGuardedRoute
      , mountRoutes
      , navigate
      , noMessages
      , mount
      ) where

import Data.Either (Either(..))
import Data.Foldable as DF
import Data.Maybe (Maybe(..))
import Data.Traversable as DT
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Exception as EE
import Effect.Console as EC
import Flame.Renderer.Internal.Mobile as FRIM
import Flame.Subscription.Internal.Listener as FSIL
import Flame.Types (AppId, ApplicationId, Html, RenderingState, Subscription)
import Prelude (class Show, Unit, bind, discard, pure, show, unit, map, ($), (<<<))

type Update model message = model → message → Tuple model (Array (Aff (Maybe message)))

-- | `Application` contains
-- | * `model` – starting model
-- | * `view` – a function to update your markup
-- | * `update` – a function to update your model
-- | * `subscribe` – list of external events
type Application model message =
      { view ∷ model → Html message
      , subscribe ∷ Array (Subscription message)
      , model ∷ model
      , update ∷ Update model message
      }

noMessages ∷ ∀ model message. model → Tuple model (Array (Aff (Maybe message)))
noMessages model = model /\ []

newtype Route = Route (Effect RenderingState)

-- | Show (or redirect to) a route
makeGuardedRoute ∷ ∀ model message id guardedId guardedMessage. Show id ⇒ Show guardedId ⇒ AppId id message → Aff (Application model message) → Effect (Maybe (AppId guardedId guardedMessage)) → Route
makeGuardedRoute appId application check = Route $ run (show appId) application (map (map show) check)

makeRoute ∷ ∀ model message id. Show id ⇒ AppId id message → Aff (Application model message) → Route
makeRoute appId application = Route $ run (show appId) application (pure Nothing)

-- | Mount a Flame application that can be fed arbitrary external messages
mount ∷ ∀ id model message. Show id ⇒ AppId id message → Application model message → Effect Unit
mount appId application = runAll [ makeRoute appId $ pure application ]

-- | Mount a Flame application with routing
mountRoutes ∷ Array Route → Effect Unit
mountRoutes = runAll

-- | Navigate to a mounted application's route
navigate ∷ ∀ id message. Show id ⇒ AppId id message → Effect Unit
navigate = FRIM.navigate <<< show

runAll ∷ Array Route → Effect Unit
runAll applications = do
      renderings ← DT.traverse (\(Route create) → create) applications
      FRIM.start renderings

run ∷ ∀ model message. ApplicationId → Aff (Application model message) → Effect (Maybe ApplicationId) → Effect RenderingState
run appId factory check = do
      FSIL.checkApplicationId appId
      FRIM.create appId (\rendering → do
            application ← factory
            let update = runUpdate application rendering
            pure
                  { model: application.model
                  , view: application.view
                  , update
                  , subscribe: do
                          FSIL.createMessageListener appId (update <<< Just)
                          DF.traverse_ (FSIL.createSubscription update) application.subscribe
                  }) check

runUpdate ∷ ∀ model message. Application model message → RenderingState → Maybe message → Effect Unit
runUpdate application rendering = case _ of
      Nothing → pure unit
      Just message → do
            state ← FRIM.getState rendering
            let updatedState /\ affs = application.update state message
            FRIM.resume rendering updatedState
            DF.for_ affs $ EA.runAff_
                  ( case _ of
                          Left error → EC.log $ EE.message error
                          Right msg → runUpdate application rendering msg
                  )
