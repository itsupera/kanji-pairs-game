module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Array exposing (Array)

import JukugoData exposing (jukugos)
import Html.Attributes exposing (selected)

-- MAIN


main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Kanji = String


type alias Model =
  { kanjis: Array Kanji
  , selected: Maybe (Int, Kanji)
  , matches: List (Kanji, Kanji)
  }


init : Model
init =
  { kanjis = initKanjis, selected = Nothing, matches = [] }

initKanjis : Array String
initKanjis =
  List.take 8 jukugos
  |> List.concatMap (\(a, b) -> [a, b])
  |> Array.fromList


-- UPDATE


type Msg
  = Clicked Int


update : Msg -> Model -> Model
update msg model =
  case msg of
    Clicked idx ->
      case Array.get idx model.kanjis of
        Nothing -> model
        Just kanji -> 
          case model.selected of
            Nothing -> {model | selected = Just (idx, kanji)}
            Just (selectedIdx, selectedKanji) ->
              if selectedIdx == idx
              then {model | selected = Nothing}  -- cancel selection
              else (
                if matchingPair model.kanjis selectedIdx idx
                then {model | selected = Nothing, matches = (updateMatches model.matches (selectedKanji, kanji)) }
                else model
              )

matchingPair : Array Kanji -> Int -> Int -> Bool
matchingPair kanjis firstSel secondSel =
    firstSel == secondSel - 1


updateMatches matches newMatch =
    matches ++ [newMatch]

-- VIEW


view : Model -> Html Msg
view model =
  div [] [viewGrid model, viewMatches model.matches]

viewGrid : Model -> Html Msg
viewGrid model =
  div
    [ style "display" "grid"
    , style "grid-template-columns" "1fr 1fr 1fr 1fr"
    , style "grid-template-rows" "1fr 1fr 1fr 1fr"
    , style "background-color" "gray"
    ]
    (model.kanjis |> Array.toIndexedList |> List.map (renderCard model.selected))

renderCard : Maybe (Int, Kanji) -> (Int, Kanji) -> Html Msg
renderCard maybeSelected (idx, kanji) =
    button
      [ onClick (Clicked idx)
      , style "margin" "5px"
      , style "background-color" (bgcolor maybeSelected idx)
      , style "font-size" "10ex"
      ]
      [ text kanji ]
bgcolor : Maybe (Int, Kanji) -> Int -> String
bgcolor maybeSelected idx =
    maybeSelected
    |> Maybe.map (\(selectedIdx, _) -> if selectedIdx == idx then "red" else "white")
    |> Maybe.withDefault "white"

viewMatches : List (Kanji, Kanji) -> Html Msg
viewMatches matches =
    div [] [matches |> List.map Debug.toString |> String.concat |> text ]