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

-- | Allows a "dynamic" application list
newtype Route = Route (∀ result. (∀ model message. ApplicationId → Application model message → Effect (Maybe ApplicationId) → result) → result)

-- | Redirect when the guard returns a different app ID; otherwise show the route
makeGuardedRoute ∷ ∀ model message id targetId targetMessage. Show id ⇒ Show targetId ⇒ AppId id message → Application model message → Effect (Maybe (AppId targetId targetMessage)) → Route
makeGuardedRoute appId application check = Route \use → use (show appId) application (map (map show) check)

makeRoute ∷ ∀ model message id. Show id ⇒ AppId id message → Application model message → Route
makeRoute appId application = makeGuardedRoute appId application (pure (Nothing ∷ Maybe (AppId id message)))

-- | Mount a Flame application that can be fed arbitrary external messages
mount ∷ ∀ id model message. Show id ⇒ AppId id message → Application model message → Effect Unit
mount appId application = runAll [ makeRoute appId application ]

-- | Mount a Flame application with routing
mountRoutes ∷ Array Route → Effect Unit
mountRoutes = runAll

-- | Navigate to a mounted application's route
navigate ∷ ∀ id message. Show id ⇒ AppId id message → Effect Unit
navigate = FRIM.navigate <<< show

runAll ∷ Array Route → Effect Unit
runAll applications = do
      renderings ← DT.traverse (\(Route use) → use run) applications
      FRIM.start renderings

run ∷ ∀ model message. ApplicationId → Application model message → Effect (Maybe ApplicationId) → Effect RenderingState
run appId application check = do
      rendering ← FRIM.create appId runUpdate application.view application.model check
      FSIL.createMessageListener appId (runUpdate rendering <<< Just)
      DF.traverse_ (FSIL.createSubscription (runUpdate rendering <<< Just)) application.subscribe
      pure rendering
      where
      runUpdate rendering = case _ of
            Nothing → pure unit
            Just message → do
                  state ← FRIM.getState rendering
                  let updatedState /\ affs = application.update state message
                  FRIM.resume rendering updatedState
                  DF.for_ affs $ EA.runAff_
                        ( case _ of
                                Left error → EC.log $ EE.message error
                                Right msg → runUpdate rendering msg
                        )
