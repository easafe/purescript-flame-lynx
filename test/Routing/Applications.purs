module Test.Routing.Applications where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class as EC
import Effect.Exception as EE
import Effect.Aff as EA
import Effect.Ref as ER
import Flame (AppId(..), Application, Route)
import Flame as F
import Flame.Html.Attribute as FHA
import Flame.Html.Element as FHE
import Flame.Subscription as FS
import Flame.Subscription.Unsafe.CustomEvent as FSUC
import Web.Event.Event (EventType(..))

data CounterMessage = Increment | Later | Fail | TellGreeting

data GreetingMessage = Append String

counterId ∷ AppId Int CounterMessage
counterId = AppId 1

greetingId ∷ AppId Int GreetingMessage
greetingId = AppId 2

counter ∷ Application Int CounterMessage
counter =
      { model: 0
      , view: \model → FHE.div [ FHA.id "counter" ]
            [ FHE.text $ show model
            , FHE.button [ FHA.id "increment", FHA.onClick Increment ] [ FHE.text "+" ]
            , FHE.button [ FHA.id "later", FHA.onClick Later ] [ FHE.text "Later" ]
            , FHE.button [ FHA.id "tell", FHA.onClick TellGreeting ] [ FHE.text "Send" ]
            , FHE.input [ FHA.id "confirm", FHA.onConfirm $ const Increment ]
            , FHE.button [ FHA.id "fail", FHA.onClick Fail ] [ FHE.text "Fail" ]
            , FHE.input [ FHA.id "ignored", FHA.createRawEvent "bindinput" <<< const $ pure Nothing ]
            , const Increment <$> FHE.input [ FHA.id "mapped", FHA.onInput identity ]
            ]
      , update: \model → case _ of
            Increment → (model + 1) /\ []
            Later → model /\ [ pure $ Just Increment, pure Nothing ]
            Fail → model /\ [ EA.throwError $ EE.error "expected test failure" ]
            TellGreeting → model /\ [ (EC.liftEffect <<< FS.send greetingId $ Append "!") $> Nothing ]
      , subscribe:
            [ FS.onCustomEvent (EventType "refresh") Increment
            , FS.onCustomEvent' (EventType "payload") $ \(eventPayload ∷ { accept ∷ Boolean }) → pure if eventPayload.accept then Just Increment else Nothing
            ]
      }

greeting ∷ Application String GreetingMessage
greeting =
      { model: "hello"
      , view: \model → FHE.div [ FHA.id "greeting" ] [ FHE.text model ]
      , update: \model (Append suffix) → (model <> suffix) /\ []
      , subscribe: [ FS.onCustomEvent (EventType "refresh") $ Append "?" ]
      }

applications ∷ Array Route
applications = [ F.makeRoute counterId $ pure counter, F.makeRoute greetingId $ pure greeting ]

main ∷ Effect Unit
main = F.mountRoutes applications

single ∷ Effect Unit
single = F.mount (AppId 6) counter

guarded ∷ Effect Unit
guarded = F.mountRoutes
      [ F.makeGuardedRoute (AppId 3) (pure counter) <<< pure <<< Just $ AppId 4
      , F.makeRoute (AppId 4) $ pure greeting
      ]

selfGuarded ∷ Effect Unit
selfGuarded = F.mountRoutes [ F.makeGuardedRoute (AppId 5) (pure counter) <<< pure <<< Just $ AppId 5 ]

goCounter ∷ Effect Unit
goCounter = F.navigate counterId

goGreeting ∷ Effect Unit
goGreeting = F.navigate greetingId

sendIncrement ∷ Effect Unit
sendIncrement = FS.send counterId Increment

refresh ∷ Effect Unit
refresh = FSUC.broadcast' $ EventType "refresh"

payload ∷ Boolean → Effect Unit
payload accept = FSUC.broadcast (EventType "payload") { accept }

goGuarded ∷ Effect Unit
goGuarded = F.navigate $ AppId 3

-- A manually completed Aff makes navigation during construction deterministic.
asynchronous ∷ Int → Effect
      { finish ∷ Effect Unit
      , fail ∷ Effect Unit
      , events ∷ Effect (Array String)
      , setAllowed ∷ Boolean → Effect Unit
      , go ∷ Effect Unit
      , leave ∷ Effect Unit
      , send ∷ String → Effect Unit
      , publish ∷ Effect Unit
      }
asynchronous id = do
      events ← ER.new []
      completion ← ER.new (\_ → pure unit)
      allowed ← ER.new false
      let
            appId = AppId id ∷ AppId Int GreetingMessage
            waiting = AppId (id + 1) ∷ AppId Int GreetingMessage
            eventType = EventType $ "async-" <> show id
            record event = ER.modify_ (_ <> [ event ]) events
            factory = do
                  application ← EA.makeAff \resolve → do
                        record "start"
                        ER.write resolve completion
                        pure EA.nonCanceler
                  EC.liftEffect $ record "finish"
                  pure application
            complete result = ER.read completion >>= (_ $ result)
      F.mountRoutes
            [ F.makeGuardedRoute appId factory do
                  record "guard"
                  canEnter ← ER.read allowed
                  pure if canEnter then Nothing else Just waiting
            , F.makeRoute waiting $ pure greeting
            ]
      pure
            { finish: complete $ Right greeting { model = "loaded", subscribe = [ FS.onCustomEvent eventType $ Append "?" ] }
            , fail: complete $ Left $ EE.error "expected factory failure"
            , events: ER.read events
            , setAllowed: flip ER.write allowed
            , go: F.navigate appId
            , leave: F.navigate waiting
            , send: FS.send appId <<< Append
            , publish: FSUC.broadcast' eventType
            }
