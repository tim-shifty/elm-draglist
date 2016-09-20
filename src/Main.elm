import Draglist exposing (init, view, update, subscriptions, const)

import Html.App as App
import Platform.Sub as Sub
import Platform.Cmd as Cmd
import Html exposing (span, button)
import Html.Events exposing (onClick)
import List

main = App.program
  { init = init <| List.map initItem ["one", "two", "three"]
  , view = view viewItem
  , update = update updateItem
  , subscriptions = subscriptions (const Sub.none) }

type alias ItemModel =
  { text : String
  , number : Int
  }

type ItemMsg = Up | Down

initItem text = ItemModel text 0
viewItem { text, number } = span []
  [ Html.text text
  , button [onClick Down] [Html.text "-"]
  , Html.text <| toString number
  , button [onClick Up] [Html.text "+"]
  ]
updateItem msg model = (
  case msg of
    Up -> {model| number = model.number + 1}
    Down -> {model| number = model.number - 1}
  , Cmd.none)
