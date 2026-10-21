module Test.Tests where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Either (Either(..))
import Data.Either as DE
import Data.Identity (Identity(..))
import Data.Foldable as DF
import Data.String.CodeUnits as DSC
import Data.Traversable as DT
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Exception as EE
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Aff.Compat (EffectFnAff)
import Effect.Aff.Compat as EAC
import Effect.Class as EC
import Flame (Html)
import Flame.Html.Attribute as FHA
import Flame.Html.Element as FHE
import Flame.Renderer.Internal.Mobile as FRIM
import Flame.Subscription as FS
import Flame.Subscription.Unsafe.CustomEvent as FSUC
import Flame.Types (NodeData, RenderingState)
import Test.Basic.Application as TBA
import Test.Functor.Basic as TFB
import Test.Subscription.EffectList (TEELMessage(..))
import Test.Subscription.EffectList as TSE
import Test.Subscription.Broadcast as TSB
import Test.Routing.Applications as TRA
import Test.Spec (Spec)
import Test.Spec as TS
import Test.Spec.Assertions as TSA
import Web.Event.Event (EventType(..))
import Web.DOM.AttrName (AttrName(..))
import Web.DOM.Element (Element)
import Web.DOM.Element as WDE
import Web.DOM.ElementId (ElementId(..))
import Web.DOM.HTMLCollection as WDH
import Web.DOM.Node as WDN
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode as WDP
import Web.HTML as WH
import Web.HTML.HTMLDocument as WHH
import Web.HTML.Window as WHW

type AsyncTest = Effect Unit → (Error → Effect Unit) → Effect Unit

tests ∷ Effect Unit
tests = case TS.collect spec of
      Identity tests → register tests
      where
      execute action success failure = void <<< EA.runAff_ (DE.either failure $ const success) $ action
      register = DF.traverse_ case _ of
            TS.Leaf name (Just (TS.Item item)) → registerTest_ name <<< execute $ item.example (\run → run unit)
            TS.Leaf name Nothing → pending_ name
            TS.Node (Left name) children → describe_ name $ register children
            TS.Node (Right cleanup) children → describe_ "cleanup scope" do
                  register children
                  afterAll_ <<< execute $ cleanup unit

spec ∷ Spec Unit
spec = do
      TS.describe "Basic applications" do
            TS.it "application" do
                  perform do
                        unsafeCreateEnviroment
                        TBA.mount
                  childrenLength ← childrenNodeLengthOf "page > view"
                  --span, input, input
                  TSA.shouldEqual 3 childrenLength

                  initial ← textContent "#text-output"
                  TSA.shouldEqual "" initial

                  tap "#cut-button"
                  current ← textContent "#text-output"
                  TSA.shouldEqual "" current

                  dispatch "#text-input" "input" "test"
                  tap "#cut-button"
                  cut ← textContent "#text-output"
                  --always remove at least one character
                  TSA.shouldSatisfy cut ((_ < 4) <<< DSC.length)

                  dispatch "#text-input" "input" "test"
                  dispatch "#text-input" "confirm" "test"
                  submitted ← textContent "#text-output"
                  TSA.shouldEqual "thanks" submitted

      TS.describe "functor" do
            TS.it "basic" do
                  perform do
                        unsafeCreateEnviroment
                        TFB.mount
                  childrenLength ← childrenNodeLengthOf "page > view"
                  --button, div
                  TSA.shouldEqual 2 childrenLength

                  tap "#add-button"
                  initial ← textContent "#text-output-0"
                  TSA.shouldEqual "0" initial

                  tap "#decrement-button-0"
                  current ← textContent "#text-output-0"
                  TSA.shouldEqual "-1" current

                  tap "#add-button"
                  tap "#increment-button-1"
                  tap "#increment-button-1"
                  current2 ← textContent "#text-output-1"
                  TSA.shouldEqual "2" current2

      TS.describe "Subscription applications" do
            TS.it "application" do
                  id ← perform do
                        unsafeCreateEnviroment
                        TSE.mount
                  output ← textContent "#text-output"
                  TSA.shouldEqual "0" output

                  perform $ FS.send id TEELDecrement
                  output2 ← textContent "#text-output"
                  TSA.shouldEqual "-1" output2

                  perform $ FS.send id TEELIncrement
                  output3 ← textContent "#text-output"
                  TSA.shouldEqual "0" output3

            TS.it "broadcast" do
                  perform do
                        unsafeCreateEnviroment
                        TSB.mount
                  output ← textContent "#text-output"
                  TSA.shouldEqual "0" output

                  perform <<< FSUC.broadcast (EventType "decrement-event") $ Just 34
                  output2 ← textContent "#text-output"
                  TSA.shouldEqual "-34" output2

                  perform $ FSUC.broadcast' (EventType "increment-event")
                  output3 ← textContent "#text-output"
                  TSA.shouldEqual "-33" output3

      TS.describe "Lynx node creation" do
            TS.it "ToClassList instances" do
                  void $ mountHtml $ FHE.div [ FHA.id "classes", FHA.class' { selected: true, disabled: false } ] []
                  classes ← EC.liftEffect $ getAttribute "#classes" "class"
                  TSA.shouldEqual "selected" classes

            TS.it "inline style and class names" do
                  void $ mountHtml $ FHE.div [ FHA.id "styled", FHA.class' "myClass", FHA.style { backgroundColor: "red", paddingTop: "2px" } ] []
                  color ← EC.liftEffect $ getStyle "#styled" "background-color"
                  padding ← EC.liftEffect $ getStyle "#styled" "padding-top"
                  classes ← EC.liftEffect $ getAttribute "#styled" "class"
                  TSA.shouldEqual "red" color
                  TSA.shouldEqual "2px" padding
                  TSA.shouldEqual "my-class" classes

            TS.it "native boolean and number properties" do
                  TSA.shouldEqual "false" <<< attributeValue_ $ FHA.disabled false
                  TSA.shouldEqual "false" <<< attributeValue_ $ FHA.readOnly false
                  TSA.shouldEqual "readonly" <<< attributeName_ $ FHA.readOnly false
                  TSA.shouldEqual "true" <<< attributeValue_ $ FHA.autoSize true
                  TSA.shouldEqual "false" <<< attributeValue_ $ FHA.autoplay false
                  TSA.shouldEqual "0" <<< attributeValue_ $ FHA.maxlength 0

            TS.it "custom elements and attributes" do
                  void $ mountHtml $ FHE.createElement "custom-view" [ FHA.id "custom", FHA.createAttribute "custom-prop" "value" ] []
                  value ← EC.liftEffect $ getAttribute "#custom" "custom-prop"
                  TSA.shouldEqual "value" value

            TS.it "SVG content" do
                  void $ mountHtml $ FHE.svg [ FHA.id "graphic", FHA.viewBox "0 0 10 10" ]
                        [ FHE.path [ FHA.d "M0 0L10 10", FHA.stroke "red" ] [] ]
                  content ← EC.liftEffect $ getAttribute "svg" "content"
                  TSA.shouldEqual "<svg id=\"graphic\" viewBox=\"0 0 10 10\" ><path d=\"M0 0L10 10\" stroke=\"red\"  /></svg>" content

      TS.describe "Lynx node update" do
            TS.it "update text nodes" do
                  state ← mountHtml $ FHE.div [ FHA.id "changing" ] [ FHE.text "before" ]
                  resumeHtml state $ FHE.div [ FHA.id "changing" ] [ FHE.text "after" ]
                  value ← textContent "#changing"
                  TSA.shouldEqual "after" value

            TS.it "inserting and removing children" do
                  state ← mountHtml $ FHE.div [ FHA.id "parent" ] []
                  resumeHtml state $ FHE.div [ FHA.id "parent" ] [ FHE.div' [ FHA.id "one" ], FHE.div' [ FHA.id "two" ] ]
                  added ← childrenNodeLengthOf "#parent"
                  TSA.shouldEqual 2 added
                  resumeHtml state $ FHE.div [ FHA.id "parent" ] [ FHE.text "replacement" ]
                  value ← textContent "#parent"
                  TSA.shouldEqual "replacement" value

            TS.it "removing attributes" do
                  state ← mountHtml $ FHE.div [ FHA.id "attributes", FHA.class' "old" ] []
                  resumeHtml state $ FHE.div [ FHA.id "attributes" ] []
                  value ← EC.liftEffect $ getAttribute "#attributes" "class"
                  TSA.shouldEqual "" value

      TS.describe "keyed" do
            TS.it "swap forward and backward" do
                  let node id = FHE.div' [ FHA.key id, FHA.id id ]
                  state ← mountHtml <<< FHE.div [ FHA.id "keyed" ] $ map node [ "one", "two", "three" ]
                  resumeHtml state <<< FHE.div [ FHA.id "keyed" ] $ map node [ "three", "two", "one" ]
                  reversed ← EC.liftEffect $ childIds "#keyed"
                  TSA.shouldEqual [ "three", "two", "one" ] reversed
                  resumeHtml state <<< FHE.div [ FHA.id "keyed" ] $ map node [ "one", "two", "three" ]
                  restored ← EC.liftEffect $ childIds "#keyed"
                  TSA.shouldEqual [ "one", "two", "three" ] restored

      TS.describe "routing" do
            TS.it "queues global messages and subscription callbacks until a guard first passes" do
                  lifecycle ← perform do
                        unsafeCreateEnviroment
                        lifecycle ← TRA.queued
                        TRA.sendQueued "first"
                        TRA.queuedPayload "payload"
                        TRA.nativeQueued "last"
                        pure lifecycle
                  beforeVisit ← EC.liftEffect lifecycle.lifecycleEvents
                  TSA.shouldEqual [] beforeVisit
                  perform TRA.goQueued
                  EAC.fromEffectFnAff settle_
                  blocked ← EC.liftEffect $ exists "#queued"
                  TSA.shouldEqual false blocked
                  beforeAllowed ← EC.liftEffect lifecycle.lifecycleEvents
                  TSA.shouldEqual [ "guard" ] beforeAllowed
                  perform do
                        lifecycle.setGuardAllowed true
                        TRA.goQueued
                  replayed ← textContent "#queued"
                  TSA.shouldEqual "firstpayloadlastnested" replayed
                  events ← EC.liftEffect lifecycle.lifecycleEvents
                  TSA.shouldEqual
                        [ "guard", "guard", "update first", "decode payload", "update payload", "update last", "update nested" ]
                        events
                  perform TRA.goWaiting
                  perform $ TRA.sendQueued "inactive"
                  whileInactive ← EC.liftEffect lifecycle.lifecycleEvents
                  TSA.shouldEqual (events <> [ "update inactive" ]) whileInactive
                  perform do
                        lifecycle.setGuardAllowed false
                        TRA.goQueued
                  EAC.fromEffectFnAff settle_
                  blockedAgain ← EC.liftEffect $ exists "#queued"
                  TSA.shouldEqual false blockedAgain
                  perform do
                        lifecycle.setGuardAllowed true
                        TRA.goQueued
                  preserved ← textContent "#queued"
                  TSA.shouldEqual "firstpayloadlastnestedinactive" preserved
                  afterReentry ← EC.liftEffect lifecycle.lifecycleEvents
                  TSA.shouldEqual (whileInactive <> [ "guard", "guard" ]) afterReentry
                  perform $ TRA.queuedPayload "again"
                  subscribedOnce ← textContent "#queued"
                  TSA.shouldEqual "firstpayloadlastnestedinactiveagainnested" subscribedOnce

            TS.it "effects, mapped events, and messages between inactive routes" do
                  perform do
                        unsafeCreateEnviroment
                        TRA.main
                  initial ← textContent "#counter > text"
                  TSA.shouldEqual "0" initial
                  tap "#increment"
                  tap "#later"
                  dispatch "#confirm" "confirm" "submitted"
                  dispatch "#ignored" "input" "ignored"
                  dispatch "#mapped" "input" "mapped"
                  updated ← textContent "#counter > text"
                  TSA.shouldEqual "4" updated
                  tap "#tell"
                  perform TRA.goGreeting
                  greeting ← textContent "#greeting"
                  TSA.shouldEqual "hello!" greeting
                  perform do
                        TRA.sendIncrement
                        TRA.refresh
                        TRA.payload false
                        TRA.payload true
                  broadcast ← textContent "#greeting"
                  TSA.shouldEqual "hello!?" broadcast
                  perform TRA.goCounter
                  counter ← textContent "#counter > text"
                  TSA.shouldEqual "7" counter
                  perform TRA.goGreeting
                  preserved ← textContent "#greeting"
                  TSA.shouldEqual "hello!?" preserved

            TS.it "redirecting guards block the protected view on every entry" do
                  perform do
                        unsafeCreateEnviroment
                        TRA.guarded
                  blocked ← EC.liftEffect $ exists "#counter"
                  TSA.shouldEqual false blocked
                  EAC.fromEffectFnAff settle_
                  greeting ← textContent "#greeting"
                  TSA.shouldEqual "hello" greeting
                  perform TRA.goGuarded
                  blockedAgain ← EC.liftEffect $ exists "#counter"
                  TSA.shouldEqual false blockedAgain
                  EAC.fromEffectFnAff settle_
                  returned ← textContent "#greeting"
                  TSA.shouldEqual "hello" returned

            TS.it "a guard targeting itself allows rendering" do
                  perform do
                        unsafeCreateEnviroment
                        TRA.selfGuarded
                  tap "#increment"
                  value ← textContent "#counter > text"
                  TSA.shouldEqual "1" value

            TS.it "single mounting, duplicate IDs, and navigation cleanup" do
                  perform do
                        unsafeCreateEnviroment
                        TRA.single
                  tap "#increment"
                  value ← textContent "#counter > text"
                  TSA.shouldEqual "1" value
                  duplicate ← EC.liftEffect $ EE.try TRA.single
                  case duplicate of
                        Left _ → pure unit
                        Right _ → TSA.fail "Duplicate application IDs must be rejected"
                  perform unsafeCreateEnviroment
                  unmounted ← EC.liftEffect $ EE.try TRA.goCounter
                  case unmounted of
                        Left _ → pure unit
                        Right _ → TSA.fail "Navigation must fail after the router unmounts"


perform ∷ ∀ a. Effect a → Aff a
perform = EAC.fromEffectFnAff <<< perform_

textContent ∷ String → Aff String
textContent selector = EC.liftEffect $ queryElement selector >>= WDN.textContent <<< WDE.toNode

childrenNodeLengthOf ∷ String → Aff Int
childrenNodeLengthOf selector = EC.liftEffect $ queryElement selector >>= WDP.childElementCount <<< WDE.toParentNode

querySelector ∷ String → Effect (Maybe Element)
querySelector selector = WH.window >>= WHW.document >>= WDP.querySelector (QuerySelector selector) <<< WHH.toParentNode

queryElement ∷ String → Effect Element
queryElement selector = querySelector selector >>= case _ of
      Just element → pure element
      Nothing → EE.throw $ "No element matching " <> selector

exists ∷ String → Effect Boolean
exists selector = DM.isJust <$> querySelector selector

childIds ∷ String → Effect (Array String)
childIds selector = queryElement selector >>= WDP.children <<< WDE.toParentNode >>= WDH.toArray >>= DT.traverse (map (\(ElementId value) → value) <<< WDE.id)

getAttribute ∷ String → String → Effect String
getAttribute selector name = map (DM.fromMaybe "") $ queryElement selector >>= WDE.getAttribute (AttrName name)

getStyle ∷ String → String → Effect String
getStyle selector name = queryElement selector >>= flip getStyle_ name

dispatch ∷ String → String → String → Aff Unit
dispatch selector name value = perform $ dispatch_ selector name value

tap ∷ String → Aff Unit
tap selector = dispatch selector "tap" ""

mountHtml ∷ Html Unit → Aff RenderingState
mountHtml html = do
      perform unsafeCreateEnviroment
      state ← EC.liftEffect <<< FRIM.create "view-test" (const <<< const $ pure unit) identity html $ pure Nothing
      perform $ FRIM.start [ state ]
      pure state

resumeHtml ∷ RenderingState → Html Unit → Aff Unit
resumeHtml state html = perform $ FRIM.resume state html


foreign import registerTest_ ∷ String → AsyncTest → Effect Unit
foreign import describe_ ∷ String → Effect Unit → Effect Unit
foreign import pending_ ∷ String → Effect Unit
foreign import afterAll_ ∷ AsyncTest → Effect Unit

foreign import settle_ ∷ EffectFnAff Unit

foreign import unsafeCreateEnviroment ∷ Effect Unit
foreign import perform_ ∷ ∀ a. Effect a → EffectFnAff a
foreign import getStyle_ ∷ Element → String → Effect String
foreign import dispatch_ ∷ String → String → String → Effect Unit
foreign import attributeValue_ ∷ ∀ message. NodeData message → String
foreign import attributeName_ ∷ ∀ message. NodeData message → String
