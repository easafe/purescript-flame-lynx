module Test.Functor.Basic where

import Prelude

import Data.Array ((!!))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect (Effect)
import Flame (Html, Update)
import Flame as F
import Flame.Html.Attribute as FHA
import Flame.Html.Element as FHE
import Flame.Types (AppId(..))

type Model = Array NestedModel

data Message = Add | Remove Int | CounterMessage Int NestedMessage

init ∷ Model
init = []

update ∷ Update Model Message
update model = F.noMessages <<< case _ of
      Add → DA.snoc model nestedInit
      Remove index → DM.fromMaybe model $ DA.deleteAt index model
      CounterMessage index message →
            case model !! index of
                  Nothing → model
                  Just model' → DM.fromMaybe model $ DA.updateAt index (nestedUpdate model' message) model

view ∷ Model → Html Message
view model = FHE.main [FHA.id "main"]
      [ FHE.button [ FHA.id "add-button", FHA.onClick Add ] [FHE.text "Add"]
      , FHE.div_ $ DA.mapWithIndex viewCounter model
      ]
      where
      viewCounter index model' = FHE.div [ FHA.style { display: "flex" } ]
            [ CounterMessage index <$> nestedView index model'
            , FHE.button [ FHA.onClick $ Remove index ] [FHE.text "Remove"]
            ]

-- | The model represents the state of the app
type NestedModel = Int

-- | This datatype is used to signal events to `update`
data NestedMessage = Increment | Decrement

nestedInit ∷ NestedModel
nestedInit = 0

nestedUpdate ∷ NestedModel → NestedMessage → NestedModel
nestedUpdate model = case _ of
      Increment → model + 1
      Decrement → model - 1

-- | `view` updates the app markup whenever the model is updated
nestedView ∷ Int → NestedModel → Html NestedMessage
nestedView index model = FHE.main [FHA.id ("main-" <> show index)]
      [ FHE.button [ FHA.id ("decrement-button-" <> show index), FHA.onClick Decrement ] [FHE.text "-"]
      , FHE.span [FHA.id ("text-output-" <> show index)] [FHE.text $ show model]
      , FHE.button [ FHA.id ("increment-button-" <> show index), FHA.onClick Increment ] [FHE.text "+"]
      ]

mount ∷ Effect Unit
mount = F.mount (AppId "functor")
      { model: init
      , subscribe: []
      , update
      , view
      }
