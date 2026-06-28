module A
  ( Message(..)
  , Model
  )
  where

import Flame.Application
import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Flame (AppId(..))
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE

data Message = B

type Model = { a :: Int }

main :: Effect Unit
main = mount (AppId 2) ({
    model : { a : 54 },
    view : \m -> HE.div [HA.class' "d", HA.onClick B, HA.style { padding: "10px;" }] [HE.text $ "OI SDSD ffds " <> show m.a],
    update : \a b -> case b of B -> a { a = 345345 } /\ [],
    subscribe : []
} :: Application Model Message)