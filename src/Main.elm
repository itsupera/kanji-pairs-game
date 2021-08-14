module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, br, a, ul, li)
import Html.Events exposing (onClick)
import Html.Attributes exposing (href, style)
import Array exposing (Array)
import Random exposing (Generator, andThen)
import Random.List exposing (choices, shuffle)
import Set exposing (Set)

import JukugoData exposing (jukugos, Kanji, matchingKanjiPair)
import JukugoData exposing (jukugosDict)
import MultiDict exposing (MultiDict)

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
init _ =
    ( initModel
    , drawInitialKanjis
    )

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


-- MODEL


type alias Model =
  { kanjis : Array Kanji
  , selected : Maybe (Int, Kanji)
  , otherSelected : Maybe (Int, Kanji)
  , matches : List (Kanji, Kanji)
  , error : Maybe String
  }


initModel : Model
initModel =
  { kanjis = Array.empty
  , selected = Nothing
  , otherSelected = Nothing
  , matches = []
  , error = Nothing
  }


-- UPDATE


type Msg
  = PickedInitialKanjis (Array Kanji)
  | Clicked Int
  | PickedNewKanjiPair (Kanji, Kanji)

withDebugLog : Msg -> Msg
withDebugLog message =
  Debug.log "Msg" message

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case model.error of
    Just _ -> (model, Cmd.none)
    Nothing ->
      case withDebugLog msg of
        PickedInitialKanjis kanjis -> pickedInitialKanjis model kanjis
        Clicked idx -> clicked model idx
        PickedNewKanjiPair pair -> pickedNewKanjiPair model pair

drawInitialKanjis : Cmd Msg
drawInitialKanjis =
    Random.List.choices 8 jukugos
    |> Random.andThen drawInitialKanjisHelper
    |> Random.generate PickedInitialKanjis

drawInitialKanjisHelper : (List (Kanji, Kanji), List (Kanji, Kanji)) -> Generator (Array Kanji)
drawInitialKanjisHelper (selected, _) =
    selected
    |> List.concatMap (\(k1, k2) -> [k1, k2])
    |> Random.List.shuffle  -- replace by Random.constant for debugging
    |> Random.map Array.fromList

pickedInitialKanjis model kanjis =
  ({model | kanjis = kanjis}, Cmd.none)

clicked model idx =
  case model.otherSelected of
    Just _ ->
      (model, Cmd.none)  -- nothing to update if second selection was made
    Nothing ->
      case Array.get idx model.kanjis of
        Nothing ->
          (model, Cmd.none)
        Just kanji -> 
          case model.selected of
            Nothing ->
              ({model | selected = Just (idx, kanji)}, Cmd.none)
            Just (selectedIdx, selectedKanji) ->
              if selectedIdx == idx
              then ({model | selected = Nothing}, Cmd.none)  -- cancel selection
              else (
                if matchingKanjiPair selectedKanji kanji
                then
                  let
                    newModel =
                      { model
                        | otherSelected = Just (idx, kanji)
                        , matches = (updateMatches model.matches (selectedKanji, kanji))
                      }
                  in
                    (
                      newModel
                      , drawKanjiPair newModel
                    )
                else (model, Cmd.none)
              )

updateMatches matches newMatch =
    matches ++ [newMatch]

drawKanjiPair : Model -> Cmd Msg
drawKanjiPair model =
    model.kanjis
    |> (Array.toList >> Set.fromList)
    |> candidateSecondKanjis
    |> Set.toList
    |> drawKanjiPairFromList

candidateSecondKanjis : Set Kanji -> Set Kanji
candidateSecondKanjis firstKanjis =
  unionMap (secondKanjisMatches firstKanjis) firstKanjis

unionMap : (comparable -> Set comparable2) -> Set comparable -> Set comparable2
unionMap f s =
    let
      g x s2 = Set.union (f x) s2
    in
      Set.foldl g Set.empty s

secondKanjisMatches : Set Kanji -> Kanji -> Set Kanji
secondKanjisMatches excluded kanji1 =
  let
    notExcluded kanji2 = not (Set.member kanji2 excluded)
  in
    MultiDict.get kanji1 jukugosDict
    |> Set.filter notExcluded

drawKanjiPairFromList : List Kanji -> Cmd Msg
drawKanjiPairFromList kanjis =
    Random.generate PickedNewKanjiPair (kanjiGenerator kanjis)

kanjiGenerator : List Kanji -> Random.Generator (Kanji, Kanji)
kanjiGenerator kanjis =
    let
      toPair (selected, _) =
        case selected of
          [kanji1, kanji2] -> (kanji1, kanji2)
          _ -> ("一", "部")  -- fallback
    in
      kanjis |> Random.List.choices 2 |> Random.map toPair

pickedNewKanjiPair : Model -> (Kanji, Kanji) -> (Model, Cmd Msg)
pickedNewKanjiPair model pair =
  ( updateWithNewKanjiPair model pair
  , Cmd.none
  )

updateWithNewKanjiPair : Model -> (Kanji, Kanji) -> Model
updateWithNewKanjiPair model (newKanji1, newKanji2) =
  case (model.selected, model.otherSelected) of
    (Just (idx1, _), Just (idx2, _)) ->
      {model
        | kanjis = replaceKanjis model.kanjis [(idx1, newKanji1), (idx2, newKanji2)]
        , selected = Nothing
        , otherSelected = Nothing
      }
    (_, _) ->
      {model | error = Just "Missing selected kanjis"}

replaceKanjis : Array Kanji -> List (Int, Kanji) -> Array Kanji
replaceKanjis =
  List.foldl (\(idx, k) ks -> Array.set idx k ks)


-- VIEW


view : Model -> Html Msg
view model =
  case model.error of
    Nothing ->
      div [] [viewGrid model, viewMatches model.matches]
    Just error ->
      viewError error

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
    let
      history = List.map matchToLi (List.reverse matches)
      matchToLi (k1, k2) = li [] [text (k1 ++ k2)]
    in
      div [] [text "Previous words:", (ul [] history) ]
viewError : String -> Html Msg
viewError error =
    div
        [ style "color" "red", style "font-size" "x-large" ]
        [ text "ERROR !"
        , br [] []
        , br [] []
        , text error
        , br [] []
        , br [] []
        , div [ style "flex-grow" "0" ] [ a [ href "" ] [ text "[Retry]" ] ]
        -- , div [ style "flex-grow" "0" ] [ a [ href "." ] [ text "[Go back]" ] ]
        ]