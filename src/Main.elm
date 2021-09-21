module Main exposing (..)

import Browser
import Html exposing (Html, button, div, img, input, text)
import Html.Attributes exposing (class, name, placeholder, src, type_)
import Html.Events exposing (onClick, onInput)



---- MODEL ----


type alias Model =
    { id : Int
    , inputText : String
    , recordList : List Record
    }


stateModel : Model
stateModel =
    { id = 1
    , inputText = ""
    , recordList = []
    }


type RecordStatus
    = Active
    | Complete


type alias Record =
    { id : Int, task : String, order : Int, status : RecordStatus }


init : ( Model, Cmd Msg )
init =
    ( stateModel, Cmd.none )



---- UPDATE ----


addRecord : Model -> Record -> Model
addRecord model record =
    { model | recordList = model.recordList ++ [ record ] }


type Msg
    = Add Model
    | InputText String



--generateId =


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add state ->
            ( addRecord state { id = model.id, task = model.inputText, order = model.id, status = Active }
            , Cmd.none
            )

        InputText text ->
            ( { model | inputText = text }, Cmd.none )



---- VIEW ----


generateHeader : Html Msg
generateHeader =
    div [ class "header" ]
        [ div [ class "header-title" ] [ text "Elm To-Do App" ]
        , img [ class "header-icon", src "./icons/check.svg" ] []
        , div [ class "header-input" ]
            [ input [ type_ "text", placeholder "Add a task", name "add-to-do", class "header-add-item-input", onInput InputText ] []
            , button [ class "add-item-btn" ]
                [ img [ class "header-add-item", src "./icons/plus-black-symbol.svg", onClick (Add stateModel) ] [] ]
            ]
        ]


view : Model -> Html Msg
view model =
    generateHeader



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
