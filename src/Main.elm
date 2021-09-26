module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, input, text)
import Html.Attributes exposing (class, name, placeholder, selected, src, type_, value)
import Html.Events exposing (onClick, onInput)
import String exposing (fromInt)



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


toggleStatus : Record -> Record
toggleStatus record =
    if record.status == Active then
        { record | status = Complete }

    else
        { record | status = Active }


toggleRecordStatus : List Record -> Int -> List Record
toggleRecordStatus records id =
    List.map
        (\rec ->
            if rec.id == id then
                toggleStatus rec

            else
                rec
        )
        records


type alias Record =
    { id : Int, task : String, order : Int, status : RecordStatus }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


addRecord : List Record -> Record -> List Record
addRecord records record =
    List.append records [ record ]


editRecordText : Record -> String -> Record
editRecordText rec text =
    { rec | task = text }


editRecordListText : List Record -> Int -> String -> List Record
editRecordListText records id text =
    List.map
        (\rec ->
            if rec.id == id then
                editRecordText rec text

            else
                rec
        )
        records


type Msg
    = Add
    | Delete Int
    | Select Int String
    | Edit Int String
    | EditText String
    | ToggleStatus Int
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

        Select id text ->
            ( { model | selectedItem = id, editText = text }, Cmd.none )

        Edit id text ->
            ( { model | recordList = editRecordListText model.recordList model.selectedItem text, editText = text, selectedItem = id }, Cmd.none )

        EditText text ->
            ( { model | recordList = editRecordListText model.recordList model.selectedItem text, editText = text }, Cmd.none )

        ToggleStatus id ->
            ( { model | recordList = toggleRecordStatus model.recordList id }, Cmd.none )

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


displayInputOrText : Model -> Record -> Html Msg
displayInputOrText model record =
    if record.id == model.selectedItem then
        input
            [ type_ "text", class "task-text", onInput EditText, value record.task ]
            []

    else
        h1
            [ onClick (Select record.id record.task) ]
            [ text record.task ]


displayRecord : Model -> Record -> Html Msg
displayRecord mod record =
    div [ class "record" ]
        [ input [ type_ "checkbox", onClick (ToggleStatus record.id) ] []

        --, input [ type_ "text", class "task-text", value record.task, onClick (Edit record.id record.task), onInput EditText ] [ text record.task ]
        , displayInputOrText mod record
        , button [ class "delete-item-btn" ]
            [ img [ class "delete-item-img", src "./icons/trash.svg", onClick (Delete record.id) ] [] ]
        ]


displayList : Model -> List Record -> Html Msg
displayList model records =
    div [ class "recordList" ]
        [ div []
            (List.map (displayRecord model) records)
        ]


view : Model -> Html Msg
view model =
    div []
        [ generateHeader
        , displayList model model.recordList
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
