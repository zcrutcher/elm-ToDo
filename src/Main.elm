module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, input, text)
import Html.Attributes exposing (class, name, placeholder, src, type_)
import Html.Events exposing (onClick, onInput)



---- MODEL ----


type alias Model =
    { id : Int
    , inputText : String
    , editText : String
    , selectedItem : Int
    , recordList : List Record
    }


initialModel : Model
initialModel =
    { id = 1
    , inputText = ""
    , editText = ""
    , selectedItem = 0
    , recordList = []
    }


type RecordStatus
    = Active
    | Complete


generateId : Model -> Model
generateId model =
    { model | id = model.id + 1 }


type alias Record =
    { id : Int, task : String, order : Int, status : RecordStatus }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


addRecord : List Record -> Record -> List Record
addRecord records record =
    List.append records [ record ]


type Msg
    = Add
    | Delete Int
    | Edit Int
    | InputText String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add ->
            ( generateId { model | recordList = addRecord model.recordList { id = model.id, task = model.inputText, order = model.id, status = Active } }
            , Cmd.none
            )

        Delete id ->
            ( { model | recordList = List.filter (\rec -> rec.id /= id) model.recordList }, Cmd.none )

        Edit id ->
            ( { model | recordList = model.recordList }, Cmd.none )

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
                [ img [ class "header-add-item", src "./icons/plus-black-symbol.svg", onClick Add ] [] ]
            ]
        ]


displayRecord : Record -> Html Msg
displayRecord record =
    div [ class "record" ]
        [ input [ type_ "checkbox" ] []
        , h1 [ class "task-text", onClick (Edit record.id) ] [ text record.task ]
        , button [ class "delete-item-btn" ]
            [ img [ class "delete-item-img", src "./icons/trash.svg", onClick (Delete record.id) ] [] ]
        ]


displayList : List Record -> Html Msg
displayList records =
    div [ class "recordList" ]
        [ div []
            (List.map displayRecord records)
        ]


view : Model -> Html Msg
view model =
    div []
        [ generateHeader
        , displayList model.recordList
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
