module Main exposing (..)

import Browser
import Browser.Dom as Dom exposing (..)
import Html exposing (Html, a, button, div, form, h3, hr, img, input, li, text, ul)
import Html.Attributes exposing (checked, class, href, id, placeholder, selected, src, title, type_, value)
import Html.Events exposing (onBlur, onClick, onInput, onSubmit)
import Html.Extra as Html exposing (nothing, viewIf)
import Platform.Cmd exposing (none)
import String exposing (trim)
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


toggleRecordStatus : List Record -> Int -> List Record
toggleRecordStatus records id =
    let
        toggleStatus : Record -> Record
        toggleStatus record =
            if record.status == Active then
                { record | status = Complete }

            else
                { record | status = Active }
    in
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
    if String.isEmpty (trim record.task) then
        records

    else
        List.append records [ record ]


editRecordText : List Record -> Int -> String -> List Record
editRecordText records id text =
    let
        editText : Record -> String -> Record
        editText rec recText =
            { rec | task = recText }
    in
    List.map
        (\rec ->
            if rec.id == id then
                editText rec text

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
    | UpdateTask


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
            ( { model | recordList = editRecordText model.recordList model.selectedItem text, editText = text }, Cmd.none )

        ToggleStatus id ->
            ( { model | recordList = toggleRecordStatus model.recordList id }, Cmd.none )

        InputText text ->
            ( { model | inputText = text }, Cmd.none )

        Blur ->
            ( { model | selectedItem = 0 }, Cmd.none )

        ClearCompleted ->
            ( { model | recordList = taskList Active model.recordList }, Cmd.none )

        UpdateTask ->
            ( { model | selectedItem = 0 }, Cmd.none )

        SetFocus result ->
            ( model, Cmd.none )



---- VIEW ----


taskForm : String -> Html Msg
taskForm taskText =
    div [ class "header" ]
        [ div [ class "header-title" ] [ text "Elm To-Do App" ]
        , div [ class "form-wrapper" ]
            [ form [ class "form", onSubmit Add ]
                [ input [ type_ "image", class "add-img", src "./icons/plus-black-symbol.svg" ]
                    [ input [ type_ "submit", class "add-item-btn" ] []
                    ]
                , input [ type_ "text", placeholder "Add a task", class "add-task", value taskText, onInput InputText ] []
                ]
            ]
        ]


displayInputOrText : Int -> Int -> String -> RecordStatus -> Html Msg
displayInputOrText selected recordId task status =
    if recordId == selected then
        form [ onSubmit UpdateTask ]
            [ input
                [ type_ "text"
                , class "task-text"
                , onInput Edit
                , onBlur Blur
                , value task
                , id (String.concat [ "input-id-", String.fromInt recordId ])
                ]
                []
            ]

    else
        div
            [ onClick (Select recordId task)
            , class (recordTaskClass status)
            , class " task-text"
            ]
            [ text task ]


taskList : RecordStatus -> List Record -> List Record
taskList status records =
    let
        statusCheck : RecordStatus -> Record -> Bool
        statusCheck stat rec =
            rec.status == stat
    in
    List.filter (statusCheck status) records


completedCheck : RecordStatus -> Bool
completedCheck status =
    status == Complete


activeCategoryLabel : Int -> Html Msg
activeCategoryLabel count =
    div [ class "label" ]
        [ h3 [ class "category-labels" ]
            [ String.concat [ "Tasks - ", String.fromInt count ] |> text ]
        ]


completeCategoryLabel : Int -> Html Msg
completeCategoryLabel count =
    div [ class "complete-label" ]
        [ h3 [ class "category-labels" ]
            [ String.concat
                [ "Completed - "
                , String.fromInt count
                ]
                |> text
            ]
        , button
            [ class "clear-completed", onClick ClearCompleted ]
            [ text "Clear Completed" ]
        ]


displayList : Int -> List Record -> Html Msg
displayList selected records =
    let
        taskCount : RecordStatus -> List Record -> Int
        taskCount stat recs =
            List.length (taskList stat recs)

        displayRecord : Int -> Record -> Html Msg
        displayRecord selectedId record =
            li [ class "record" ]
                [ input
                    [ type_ "checkbox"
                    , onClick (ToggleStatus record.id)
                    , class "round-checkbox"
                    , checked (completedCheck record.status)
                    ]
                    []
                , displayInputOrText
                    selectedId
                    record.id
                    record.task
                    record.status
                , button [ class "delete-item-btn" ]
                    [ img
                        [ class "delete-item-img"
                        , src "./icons/trash.svg"
                        , onClick (Delete record.id)
                        ]
                        []
                    ]
                ]
    in
    div [ class "holder" ]
        [ viewIf
            (taskCount Active records > 0)
            (taskCount Active records |> activeCategoryLabel)
        , ul []
            (List.map
                (displayRecord selected)
                (taskList Active records)
            )
        , viewIf
            (taskCount Complete records > 0)
            (taskCount Complete records |> completeCategoryLabel)
        , ul [] (List.map (displayRecord selected) (taskList Complete records))
        ]


footer : Html Msg
footer =
    div [ class "footer" ]
        [ div []
            [ text "Icons made by "
            , a [ href "https://www.flaticon.com/authors/feen", title "feen" ] [ text "feen " ]
            , text "from "
            , a [ href "https://www.flaticon.com/", title "Flaticon" ] [ text "www.flaticon.com" ]
            ]
        , div []
            [ text "Icons made by "
            , a [ href "https://www.flaticon.com/authors/dave-gandy", title "Dave Gandy" ] [ text "Dave Gandy " ]
            , text "from "
            , a [ href "https://www.flaticon.com/", title "Flaticon" ] [ text "www.flaticon.com" ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "bod" ]
            [ taskForm model.inputText
            , displayList model.selectedItem
                model.recordList
            ]
        , footer
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
