module Test.Basic.Application (mount) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as DS
import Data.String.CodeUnits as DSC
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as EC
import Effect.Random as ER
import Flame (Html)
import Flame as F
import Flame.Html.Attribute as FHA
import Flame.Html.Element as FHE
import Flame.Types (AppId(..))

type Model = String

data Message = Current String | Cut | Submit

update ∷ Model → Message → Tuple Model (Array (Aff (Maybe Message)))
update model = case _ of
      Cut → model /\
            [ Just <<< Current <$> cut model
            ]
      Submit → "thanks" /\ []
      Current text → text /\ []
      where
      cut text = do
            amount ← EC.liftEffect <<< ER.randomInt 1 $ DSC.length text
            pure $ DS.drop amount text

view ∷ Model → Html Message
view model = FHE.main_
      [ FHE.span [ FHA.id "text-output" ] [FHE.text model]
      ,
        --we add extra events for each input to test if the correct message is used
        FHE.input [ FHA.id "text-input", FHA.type' "text", FHA.onInput Current, FHA.onFocus Cut, FHA.onConfirm (const Submit) ]
      , FHE.input [ FHA.id "cut-button", FHA.type' "button", FHA.onClick Cut, FHA.onFocus (Current "") ]
      ]

mount ∷ Effect Unit
mount = F.mount (AppId "basic")
      { model: ""
      , subscribe: []
      , update
      , view
      }
