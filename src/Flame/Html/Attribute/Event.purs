-- | Definition of HTML events that can be fired from views
module Flame.Html.Event (EventName, ToEvent, ToRawEvent, ToMaybeEvent, ToSpecialEvent, createEvent, createEventMessage, createRawEvent, onBlur, onBlur', onCheck, onClick, onClick', onChange, onChange', onContextmenu, onContextmenu', onDblclick, onDblclick', onDrag, onDrag', onDragend, onDragend', onDragenter, onDragenter', onDragleave, onDragleave', onDragover, onDragover', onDragstart, onDragstart', onDrop, onDrop', onError, onError', onFocus, onFocus', onFocusin, onFocusin', onFocusout, onFocusout', onInput, onInput', onKeydown, onKeydown', onKeypress, onKeypress', onKeyup, onKeyup', onMousedown, onMousedown', onMouseenter, onMouseenter', onMouseleave, onMouseleave', onMousemove, onMousemove', onMouseout, onMouseout', onLoad, onLoad', onUnload, onUnload', onMouseover, onMouseover', onMouseup, onMouseup', onReset, onReset', onScroll, onScroll', onSelect, onSelect', onSubmit, onSubmit', onWheel, onWheel') where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as FU
import Flame.Types (NodeData, Key)
import Web.Event.Event (Event)

type EventName = String

type ToEvent message = message → NodeData message

type ToRawEvent message = (Event → message) → NodeData message

type ToMaybeEvent message = (Event → Maybe message) → NodeData message

type ToSpecialEvent message t = (t → message) → NodeData message

--this way we dont need to worry about every possible element type
foreign import nodeValue_ ∷ EffectFn1 Event String
foreign import checkedValue_ ∷ EffectFn1 Event Boolean
foreign import preventDefault_ ∷ EffectFn1 Event Unit
foreign import key_ ∷ EffectFn1 Event Key
foreign import selection_ ∷ EffectFn1 Event String

nodeValue ∷ Event → Effect String
nodeValue = FU.runEffectFn1 nodeValue_

checkedValue ∷ Event → Effect Boolean
checkedValue = FU.runEffectFn1 checkedValue_

preventDefault ∷ Event → Effect Unit
preventDefault = FU.runEffectFn1 preventDefault_

key ∷ Event → Effect String
key = FU.runEffectFn1 key_

selection ∷ Event → Effect String
selection = FU.runEffectFn1 selection_

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

onScroll ∷ ∀ message. ToEvent message
onScroll = createEvent "bindscroll"

onScroll' ∷ ∀ message. ToRawEvent message
onScroll' = createEventMessage "bindscroll"

onClick ∷ ∀ message. ToEvent message
onClick = createEvent "bindtap"

onClick' ∷ ∀ message. ToRawEvent message
onClick' = createEventMessage "bindtap"

onLoad ∷ ∀ message. ToEvent message
onLoad = createEvent "bindload"

onLoad' ∷ ∀ message. ToRawEvent message
onLoad' = createEventMessage "bindload"

onUnload ∷ ∀ message. ToEvent message
onUnload = createEvent "bindunload"

onUnload' ∷ ∀ message. ToRawEvent message
onUnload' = createEventMessage "bindunload"

onChange ∷ ∀ message. ToEvent message
onChange = createEvent "bindchange"

onChange' ∷ ∀ message. ToRawEvent message
onChange' = createEventMessage "bindchange"

-- | This event fires when the value of an input, select, textarea, contenteditable or designMode on elements changes
onInput ∷ ∀ message. ToSpecialEvent message String
onInput constructor = createRawEvent "input" handler
      where
      handler event = Just <<< constructor <$> nodeValue event

onInput' ∷ ∀ message. ToRawEvent message
onInput' = createEventMessage "bindinput"

-- | Helper for `input` event of checkboxes and radios
onCheck ∷ ∀ message. ToSpecialEvent message Boolean
onCheck constructor = createRawEvent "input" handler
      where
      handler event = Just <<< constructor <$> checkedValue event

onSubmit ∷ ∀ message. ToEvent message
onSubmit message = createRawEvent "submit" handler
      where
      handler event = do
            preventDefault event
            pure $ Just message

onSubmit' ∷ ∀ message. ToRawEvent message
onSubmit' constructor = createRawEvent "submit" handler
      where
      handler event = do
            preventDefault event
            pure <<< Just $ constructor event

onFocus ∷ ∀ message. ToEvent message
onFocus = createEvent "bindfocus"

onFocus' ∷ ∀ message. ToRawEvent message
onFocus' = createEventMessage "bindfocus"

onFocusin ∷ ∀ message. ToEvent message
onFocusin = createEvent "bindfocusin"

onFocusin' ∷ ∀ message. ToRawEvent message
onFocusin' = createEventMessage "bindfocusin"

onFocusout ∷ ∀ message. ToEvent message
onFocusout = createEvent "bindfocusout"

onFocusout' ∷ ∀ message. ToRawEvent message
onFocusout' = createEventMessage "bindfocusout"

onBlur ∷ ∀ message. ToEvent message
onBlur = createEvent "bindblur"

onBlur' ∷ ∀ message. ToRawEvent message
onBlur' = createEventMessage "bindblur"

onReset ∷ ∀ message. ToEvent message
onReset = createEvent "bindreset"

onReset' ∷ ∀ message. ToRawEvent message
onReset' = createEventMessage "bindreset"

onKeydown ∷ ∀ message. ToSpecialEvent message (Tuple Key String)
onKeydown constructor = createRawEvent "keydown" (keyInput constructor)

onKeydown' ∷ ∀ message. ToRawEvent message
onKeydown' = createEventMessage "bindkeydown"

onKeypress ∷ ∀ message. ToSpecialEvent message (Tuple Key String)
onKeypress constructor = createRawEvent "keypress" (keyInput constructor)

onKeypress' ∷ ∀ message. ToRawEvent message
onKeypress' = createEventMessage "bindkeypress"

onKeyup ∷ ∀ message. ToSpecialEvent message (Tuple Key String)
onKeyup constructor = createRawEvent "keyup" (keyInput constructor)

onKeyup' ∷ ∀ message. ToRawEvent message
onKeyup' = createEventMessage "bindkeyup"

keyInput ∷ ∀ message. (Tuple Key String → message) → Event → Effect (Maybe message)
keyInput constructor event = do
      down ← key event
      value ← nodeValue event
      pure <<< Just <<< constructor $ Tuple down value

onContextmenu ∷ ∀ message. ToEvent message
onContextmenu = createEvent "bindcontextmenu"

onContextmenu' ∷ ∀ message. ToRawEvent message
onContextmenu' = createEventMessage "bindcontextmenu"

onDblclick ∷ ∀ message. ToEvent message
onDblclick = createEvent "binddblclick"

onDblclick' ∷ ∀ message. ToRawEvent message
onDblclick' = createEventMessage "binddblclick"

onMousedown ∷ ∀ message. ToEvent message
onMousedown = createEvent "bindmousedown"

onMousedown' ∷ ∀ message. ToRawEvent message
onMousedown' = createEventMessage "bindmousedown"

onMouseenter ∷ ∀ message. ToEvent message
onMouseenter = createEvent "bindmouseenter"

onMouseenter' ∷ ∀ message. ToRawEvent message
onMouseenter' = createEventMessage "bindmouseenter"

onMouseleave ∷ ∀ message. ToEvent message
onMouseleave = createEvent "bindmouseleave"

onMouseleave' ∷ ∀ message. ToRawEvent message
onMouseleave' = createEventMessage "bindmouseleave"

onMousemove ∷ ∀ message. ToEvent message
onMousemove = createEvent "bindmousemove"

onMousemove' ∷ ∀ message. ToRawEvent message
onMousemove' = createEventMessage "bindmousemove"

onMouseover ∷ ∀ message. ToEvent message
onMouseover = createEvent "bindmouseover"

onMouseover' ∷ ∀ message. ToRawEvent message
onMouseover' = createEventMessage "bindmouseover"

onMouseout ∷ ∀ message. ToEvent message
onMouseout = createEvent "bindmouseout"

onMouseout' ∷ ∀ message. ToRawEvent message
onMouseout' = createEventMessage "bindmouseout"

onMouseup ∷ ∀ message. ToEvent message
onMouseup = createEvent "bindmouseup"

onMouseup' ∷ ∀ message. ToRawEvent message
onMouseup' = createEventMessage "bindmouseup"

onSelect ∷ ∀ message. ToSpecialEvent message String
onSelect constructor = createRawEvent "select" handler
      where
      handler event = Just <<< constructor <$> selection event

onSelect' ∷ ∀ message. ToRawEvent message
onSelect' = createEventMessage "bindselect"

onWheel ∷ ∀ message. ToEvent message
onWheel = createEvent "bindwheel"

onWheel' ∷ ∀ message. ToRawEvent message
onWheel' = createEventMessage "bindwheel"

onDrag ∷ ∀ message. ToEvent message
onDrag = createEvent "binddrag"

onDrag' ∷ ∀ message. ToRawEvent message
onDrag' = createEventMessage "binddrag"

onDragend ∷ ∀ message. ToEvent message
onDragend = createEvent "binddragend"

onDragend' ∷ ∀ message. ToRawEvent message
onDragend' = createEventMessage "binddragend"

onDragenter ∷ ∀ message. ToEvent message
onDragenter = createEvent "binddragenter"

onDragenter' ∷ ∀ message. ToRawEvent message
onDragenter' = createEventMessage "binddragenter"

onDragstart ∷ ∀ message. ToEvent message
onDragstart = createEvent "binddragstart"

onDragstart' ∷ ∀ message. ToRawEvent message
onDragstart' = createEventMessage "binddragstart"

onDragleave ∷ ∀ message. ToEvent message
onDragleave = createEvent "binddragleave"

onDragleave' ∷ ∀ message. ToRawEvent message
onDragleave' = createEventMessage "binddragleave"

onDragover ∷ ∀ message. ToEvent message
onDragover = createEvent "binddragover"

onDragover' ∷ ∀ message. ToRawEvent message
onDragover' = createEventMessage "binddragover"

onDrop ∷ ∀ message. ToEvent message
onDrop = createEvent "binddrop"

onDrop' ∷ ∀ message. ToRawEvent message
onDrop' = createEventMessage "binddrop"

onError ∷ ∀ message. ToEvent message
onError = createEvent "binderror"

onError' ∷ ∀ message. ToRawEvent message
onError' = createEventMessage "binderror"