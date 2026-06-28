module Flame.Internal.Dom
  ( checkedValue
  , key
  , nodeValue
  , preventDefault
  , selection
  )
  where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as FU
import Flame.Types (Key)
import Web.Event.Internal.Types (Event)

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
