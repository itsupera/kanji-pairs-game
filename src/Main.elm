module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)

import JukugoData exposing (jukugos)

-- MAIN


main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Card = 
  { kanji: String
  , selected: Bool
  }

defaultCard : Card
defaultCard =
  { kanji = "水"
  , selected = False
  }

type alias Model =
  {
  cards : List Card
  }


init : Model
init =
  { cards = List.map (\kanji -> {kanji = kanji, selected = False}) initKanjis }

initKanjis : List String
initKanjis =
  List.take 8 jukugos
  |> List.concatMap (\x -> String.split "" x)

-- UPDATE


type Msg
  = Clicked Card


update : Msg -> Model -> Model
update msg model =
  case msg of
    Clicked _ ->
      model



-- VIEW


view : Model -> Html Msg
view model =
  div
    [ style "display" "grid"
    , style "grid-template-columns" "1fr 1fr 1fr 1fr"
    , style "grid-template-rows" "1fr 1fr 1fr 1fr"
    , style "background-color" "gray"
    ]
    (List.map renderCard model.cards)

renderCard : Card -> Html Msg
renderCard card =
    button
      [ onClick (Clicked card)
      , style "margin" "5px"
      , style "background-color" "white"
      , style "font-size" "10ex"
      ]
      [ text card.kanji ]