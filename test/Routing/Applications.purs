module Test.Routing.Applications where

import Prelude

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
applications = [ F.makeRoute counterId counter, F.makeRoute greetingId greeting ]

main ∷ Effect Unit
main = F.mountRoutes applications

single ∷ Effect Unit
single = F.mount (AppId 6) counter

guarded ∷ Effect Unit
guarded = F.mountRoutes
      [ F.makeGuardedRoute (AppId 3) counter <<< pure <<< Just $ AppId 4
      , F.makeRoute (AppId 4) greeting
      ]

selfGuarded ∷ Effect Unit
selfGuarded = F.mountRoutes [ F.makeGuardedRoute (AppId 5) counter <<< pure <<< Just $ AppId 5 ]

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

queuedId ∷ AppId Int GreetingMessage
queuedId = AppId 7

waitingId ∷ AppId Int GreetingMessage
waitingId = AppId 8

queued ∷ Effect { lifecycleEvents ∷ Effect (Array String), setGuardAllowed ∷ Boolean → Effect Unit }
queued = do
      events ← ER.new []
      guardAllowed ← ER.new false
      let recordLifecycle event = ER.modify_ (_ <> [ event ]) events
      F.mountRoutes
            [ F.makeRoute waitingId greeting
            , F.makeGuardedRoute queuedId
                  { model: ""
                  , view: \model → FHE.div [ FHA.id "queued" ] [ FHE.text model ]
                  , update: \model (Append suffix) → (model <> suffix) /\
                        [ EC.liftEffect (recordLifecycle $ "update " <> suffix) $> Nothing ]
                  , subscribe:
                        [ FS.onCustomEvent' (EventType "queued-payload") $ \(eventPayload ∷ { value ∷ String }) → do
                                recordLifecycle $ "decode " <> eventPayload.value
                                FS.send queuedId $ Append "nested"
                                pure <<< Just $ Append eventPayload.value
                        ]
                  }
                  do
                        recordLifecycle "guard"
                        allowed ← ER.read guardAllowed
                        pure if allowed then Nothing else Just waitingId
            ]
      pure { lifecycleEvents: ER.read events, setGuardAllowed: flip ER.write guardAllowed }

goQueued ∷ Effect Unit
goQueued = F.navigate queuedId

goWaiting ∷ Effect Unit
goWaiting = F.navigate waitingId

sendQueued ∷ String → Effect Unit
sendQueued = FS.send queuedId <<< Append

nativeQueued ∷ String → Effect Unit
nativeQueued = nativeQueued_ <<< Append

queuedPayload ∷ String → Effect Unit
queuedPayload value = FSUC.broadcast (EventType "queued-payload") { value }

foreign import nativeQueued_ ∷ GreetingMessage → Effect Unit
