module Main exposing (..)

import Browser
import Browser.Dom as Dom exposing (..)
import Html exposing (Html, button, div, form, h3, img, input, text)
import Html.Attributes exposing (class, id, name, placeholder, selected, src, type_, value)
import Html.Events exposing (onBlur, onClick, onInput, onSubmit)
import Task



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


recordTaskClass : RecordStatus -> String
recordTaskClass status =
    case status of
        Complete ->
            "task-complete"

        Active ->
            "task"


focusInputBox : Int -> Cmd Msg
focusInputBox id =
    Task.attempt FocusTest (Dom.focus (String.concat [ "input-id-", String.fromInt id ]))



--recordTaskClass : RecordStatus -> String


type Msg
    = Add
    | Delete Int
    | Select Int String
    | Edit String
    | ToggleStatus Int
    | InputText String
    | Blur
    | SetFocus Int
    | FocusTest (Result Dom.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add ->
            ( generateId
                { model
                    | recordList = addRecord model.recordList { id = model.id, task = model.inputText, order = model.id, status = Active }
                    , inputText = ""
                }
            , Cmd.none
            )

        Delete id ->
            ( { model | recordList = List.filter (\rec -> rec.id /= id) model.recordList }, Cmd.none )

        Select id text ->
            ( { model | selectedItem = id, editText = text }, focusInputBox id )

        -- Edit id text ->
        --     ( { model | recordList = editRecordListText model.recordList model.selectedItem text, editText = text, selectedItem = id }, Cmd.none )
        Edit text ->
            ( { model | recordList = editRecordListText model.recordList model.selectedItem text, editText = text }, Cmd.none )

        ToggleStatus id ->
            ( { model | recordList = toggleRecordStatus model.recordList id }, Cmd.none )

        InputText text ->
            ( { model | inputText = text }, Cmd.none )

        Blur ->
            ( { model | selectedItem = 0 }, Cmd.none )

        SetFocus id ->
            ( model, focusInputBox id )

        FocusTest result ->
            ( model, Cmd.none )



---- VIEW ----


taskForm : Model -> Html Msg
taskForm model =
    div [ class "header" ]
        [ div [ class "header-title" ] [ text "Elm To-Do App" ]
        , img [ class "header-icon", src "./icons/check.svg" ] []
        , form [ class "add-form", onSubmit Add ]
            [ input [ type_ "image", class "header-add-item testAddImage", src "./icons/plus-black-symbol.svg" ]
                [ input [ type_ "submit", class "add-item-btn" ] []
                ]
            , input [ type_ "text", placeholder "Add a task", class "add-task", value model.inputText, onInput InputText ] []
            ]
        ]


generateHeader : Model -> Html Msg
generateHeader model =
    div [ class "header" ]
        [ div [ class "header-title" ] [ text "Elm To-Do App" ]
        , img [ class "header-icon", src "./icons/check.svg" ] []
        , div [ class "header-input" ]
            [ input
                [ type_ "text"
                , placeholder "Add a task"
                , name "add-to-do"
                , class "header-add-item-input"
                , onInput InputText
                , value model.inputText
                ]
                []
            , button [ class "add-item-btn" ]
                [ img [ class "header-add-item", src "./icons/plus-black-symbol.svg", onClick Add ] [] ]
            ]
        ]


displayInputOrText : Int -> Record -> Html Msg
displayInputOrText selected record =
    if record.id == selected then
        input
            [ type_ "text", class "task-text", onInput Edit, onBlur Blur, value record.task, id (String.concat [ "input-id-", String.fromInt record.id ]) ]
            []

    else
        h3
            [ onClick (Select record.id record.task)
            , class (recordTaskClass record.status)
            ]
            [ text record.task ]


displayRecord : Model -> Record -> Html Msg
displayRecord model record =
    div [ class "record" ]
        [ input [ type_ "checkbox", onClick (ToggleStatus record.id) ] []
        , displayInputOrText model.selectedItem record
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
        [ taskForm model --generateHeader model
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
