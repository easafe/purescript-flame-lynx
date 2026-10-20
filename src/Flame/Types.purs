-- | Types common to Flame modules
module Flame.Types
      ( PreApplication
      , ToNodeData
      , Tag
      , Key
      , AppId(..)
      , ApplicationId
      , Subscription
      , RenderingState
      , EventName
      , NodeData
      , Html
      ) where

import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Foreign (Foreign)
import Prelude (class Functor, class Show, map, show)

-- | `PreApplication` contains
-- | * `model` – the initial model
-- | * `view` – a function to update your markup
type PreApplication model message =
      { model ∷ model
      , view ∷ model → Html message
      }

type ApplicationId = String

newtype AppId ∷ ∀ k. Type → k → Type
newtype AppId a b = AppId a

instance Show a ⇒ Show (AppId a b) where
      show (AppId id) = show id

type EventName = String

-- | Subscriptions to Lynx global events
type Subscription message = EventName /\ (Foreign → Effect (Maybe message))

type ToNodeData value = ∀ message. value → NodeData message

type Tag = String
type Key = String

-- | FFI class that keeps track of Lynx rendering
foreign import data RenderingState ∷ Type

-- | Attributes and properties of virtual nodes
foreign import data NodeData ∷ Type → Type

--Html can actually be typed, but since it is only used in FFI code, I don't think it'd be very useful
-- | The type of virtual nodes
foreign import data Html ∷ Type → Type

--we support events that are not fired if the message is Nothing
foreign import messageMapper ∷ ∀ message mapped. (Maybe message → Maybe mapped) → Html message → Html mapped

instance Functor Html where
      map f html = messageMapper (map f) html
