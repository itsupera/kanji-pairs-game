module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Array exposing (Array)

import JukugoData exposing (jukugos, Kanji, matchingKanjiPair)

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init query =
    ( initModel
    , Cmd.none
    )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- MODEL


type alias Model =
  { kanjis: Array Kanji
  , selected: Maybe (Int, Kanji)
  , matches: List (Kanji, Kanji)
  }


initModel : Model
initModel =
  { kanjis = initKanjis, selected = Nothing, matches = [] }

initKanjis : Array String
initKanjis =
  List.take 8 jukugos
  |> List.concatMap (\(a, b) -> [a, b])
  |> Array.fromList


-- UPDATE


type Msg
  = Clicked Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Clicked idx ->
      case Array.get idx model.kanjis of
        Nothing -> (model, Cmd.none)
        Just kanji -> 
          case model.selected of
            Nothing -> ({model | selected = Just (idx, kanji)}, Cmd.none)
            Just (selectedIdx, selectedKanji) ->
              if selectedIdx == idx
              then ({model | selected = Nothing}, Cmd.none)  -- cancel selection
              else (
                if matchingKanjiPair selectedKanji kanji
                then ({model | selected = Nothing, matches = (updateMatches model.matches (selectedKanji, kanji)) }, Cmd.none)
                else (model, Cmd.none)
              )

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