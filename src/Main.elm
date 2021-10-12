module Main exposing (..)

import Browser
import Browser.Dom as Dom exposing (..)
import Html exposing (Attribute, Html, button, div, form, h3, h5, hr, img, input, text)
import Html.Attributes exposing (checked, class, id, placeholder, selected, src, type_, value)
import Html.Events exposing (onBlur, onClick, onInput, onSubmit)
import Platform.Cmd exposing (none)
import Task



---- MODEL ----


type alias Model =
    { id : Int
    , inputText : String
    , editText : String
    , selectedItem : Int
    , recordList : List Record
    }


type alias Record =
    { id : Int, task : String, order : Int, status : RecordStatus }


type RecordStatus
    = Active
    | Complete


initialModel : Model
initialModel =
    { id = 1
    , inputText = ""
    , editText = ""
    , selectedItem = 0
    , recordList = []
    }


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
    Task.attempt SetFocus (Dom.focus (String.concat [ "input-id-", String.fromInt id ]))


type Msg
    = Add
    | Delete Int
    | Select Int String
    | Edit String
    | ToggleStatus Int
    | InputText String
    | Blur
    | SetFocus (Result Dom.Error ())
    | ClearCompleted


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

        Edit text ->
            ( { model | recordList = editRecordListText model.recordList model.selectedItem text, editText = text }, Cmd.none )

        ToggleStatus id ->
            ( { model | recordList = toggleRecordStatus model.recordList id }, Cmd.none )

        InputText text ->
            ( { model | inputText = text }, Cmd.none )

        Blur ->
            ( { model | selectedItem = 0 }, Cmd.none )

        ClearCompleted ->
            ( { model | recordList = activeTasks model.recordList }, Cmd.none )

        SetFocus result ->
            ( model, Cmd.none )



---- VIEW ----


taskForm : String -> Html Msg
taskForm taskText =
    div [ class "header" ]
        [ div [ class "header-title" ] [ text "Elm To-Do App" ]
        , img [ class "header-icon", src "./icons/check.svg" ] []
        , form [ class "add-form", onSubmit Add ]
            [ input [ type_ "image", class "header-add-item testAddImage", src "./icons/plus-black-symbol.svg" ]
                [ input [ type_ "submit", class "add-item-btn" ] []
                ]
            , input [ type_ "text", placeholder "Add a task", class "add-task", value taskText, onInput InputText ] []
            ]
        ]


displayInputOrText : Int -> Int -> String -> RecordStatus -> Html Msg
displayInputOrText selected recordId task status =
    if recordId == selected then
        input
            [ type_ "text", class "task-text", onInput Edit, onBlur Blur, value task, id (String.concat [ "input-id-", String.fromInt recordId ]) ]
            []

    else
        div
            [ onClick (Select recordId task)
            , class (recordTaskClass status)
            , class " task-text"
            ]
            [ text task ]


completeTaskCheck : Record -> Bool
completeTaskCheck record =
    record.status == Complete


activeTaskCheck : Record -> Bool
activeTaskCheck record =
    record.status == Active


completeTasks : List Record -> List Record
completeTasks records =
    List.filter completeTaskCheck records


activeTasks : List Record -> List Record
activeTasks records =
    List.filter activeTaskCheck records


completedCheck : RecordStatus -> Bool
completedCheck status =
    status == Complete


activeCategoryLabel : List Record -> Html Msg
activeCategoryLabel records =
    if List.length (activeTasks records) > 0 then
        div []
            [ h3 [ class "category-labels" ] [ text "Tasks" ]
            , hr [] []
            ]

    else
        text ""


completeCategoryLabel : List Record -> Html Msg
completeCategoryLabel records =
    if List.length (completeTasks records) > 0 then
        div []
            [ h3 [ class "category-labels" ] [ text "Completed" ]
            , h5
                [ onClick ClearCompleted
                , class "clear-completed"
                ]
                [ text "Clear Completed Tasks" ]
            , hr [] []
            ]

    else
        text ""


displayList : Int -> List Record -> Html Msg
displayList selected records =
    let
        displayRecord : Int -> Record -> Html Msg
        displayRecord selectedId record =
            div [ class "record" ]
                [ input
                    [ type_ "checkbox"
                    , onClick (ToggleStatus record.id)
                    , class "round-checkbox"
                    , checked (completedCheck record.status)
                    ]
                    []
                , displayInputOrText selectedId record.id record.task record.status
                , button [ class "delete-item-btn" ]
                    [ img [ class "delete-item-img", src "./icons/trash.svg", onClick (Delete record.id) ] [] ]
                ]
    in
    div [ class "recordList" ]
        [ activeCategoryLabel records
        , div [ class "active-task-list" ]
            (List.map (displayRecord selected) (activeTasks records))
        , completeCategoryLabel records
        , div [ class "complete-task-list" ]
            (List.map (displayRecord selected) (completeTasks records))
        ]


view : Model -> Html Msg
view model =
    div []
        [ taskForm model.inputText
        , displayList model.selectedItem model.recordList
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
