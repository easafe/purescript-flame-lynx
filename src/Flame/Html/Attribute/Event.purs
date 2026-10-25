-- | Events supported by Lynx elements
module Flame.Html.Event (EventName, ToEvent, ToRawEvent, ToMaybeEvent, ToSpecialEvent, createEvent, createEventMessage, createRawEvent, onBlur, onBlur', onClick, onClick', onChange, onChange', onError, onError', onFocus, onFocus', onInput, onInput', onLoad, onLoad', onScroll, onScroll', onConfirm, onConfirm') where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Flame.Types (NodeData)
import Web.Event.Event (Event)

type EventName = String

type ToEvent message = message → NodeData message

type ToRawEvent message = (Event → message) → NodeData message

type ToMaybeEvent message = (Event → Maybe message) → NodeData message

type ToSpecialEvent message t = (t → message) → NodeData message

foreign import nodeValue_ ∷ EffectFn1 Event String

nodeValue ∷ Event → Effect String
nodeValue = EU.runEffectFn1 nodeValue_

foreign import createEvent_ ∷ ∀ message f. f → EventName → message → (NodeData message)

-- | Raises the given `message` for the event
createEvent ∷ ∀ message. EventName → message → NodeData message
createEvent name message = createEvent_ Just name message

foreign import createRawEvent_ ∷ ∀ message. EventName → (Event → Effect (Maybe message)) → (NodeData message)

-- | Raises the given `message` for the given event, but also supplies the event itself
createRawEvent ∷ ∀ message. EventName → (Event → Effect (Maybe message)) → NodeData message
createRawEvent name handler = createRawEvent_ name handler

-- | Helper for `message`s that expect an event
createEventMessage ∷ ∀ message. EventName → (Event → message) → NodeData message
createEventMessage eventName constructor = createRawEvent eventName (pure <<< Just <<< constructor)

onBlur ∷ ∀ message. ToEvent message
onBlur = createEvent "bindblur"

onBlur' ∷ ∀ message. ToRawEvent message
onBlur' = createEventMessage "bindblur"

onClick ∷ ∀ message. ToEvent message
onClick = createEvent "bindtap"

onClick' ∷ ∀ message. ToRawEvent message
onClick' = createEventMessage "bindtap"

onChange ∷ ∀ message. ToEvent message
onChange = createEvent "bindchange"

onChange' ∷ ∀ message. ToRawEvent message
onChange' = createEventMessage "bindchange"

onError ∷ ∀ message. ToEvent message
onError = createEvent "binderror"

onError' ∷ ∀ message. ToRawEvent message
onError' = createEventMessage "binderror"

onFocus ∷ ∀ message. ToEvent message
onFocus = createEvent "bindfocus"

onFocus' ∷ ∀ message. ToRawEvent message
onFocus' = createEventMessage "bindfocus"

onInput ∷ ∀ message. ToSpecialEvent message String
onInput constructor = createRawEvent "bindinput" handler
      where
      handler event = Just <<< constructor <$> nodeValue event

onInput' ∷ ∀ message. ToRawEvent message
onInput' = createEventMessage "bindinput"

onLoad ∷ ∀ message. ToEvent message
onLoad = createEvent "bindload"

onLoad' ∷ ∀ message. ToRawEvent message
onLoad' = createEventMessage "bindload"

onScroll ∷ ∀ message. ToEvent message
onScroll = createEvent "bindscroll"

onScroll' ∷ ∀ message. ToRawEvent message
onScroll' = createEventMessage "bindscroll"

-- | The input keyboard's confirm action supplies the current input value
onConfirm ∷ ∀ message. ToSpecialEvent message String
onConfirm constructor = createRawEvent "bindconfirm" handler
      where
      handler event = Just <<< constructor <$> nodeValue event

onConfirm' ∷ ∀ message. ToRawEvent message
onConfirm' = createEventMessage "bindconfirm"
