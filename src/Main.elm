port module Main exposing (..)

import Html
import Html.Attributes
import Html.Events
import Keyboard.Event
import Markdown
import Process
import Regex
import Task
import Time
import Window.Events


---- MODEL ----


type alias Model =
    { content : List String
    , tool : Tool
    , selectedParagraph : Int
    }


type Tool
    = Title
    | Quote
    | Edit
    | Raw


type alias StoredContent =
    List String


init : StoredContent -> ( Model, Cmd Msg )
init storedContent =
    let
        tool =
            if storedContent == [] then
                Raw
            else
                Title
    in
        ( { content = storedContent
          , tool = tool
          , selectedParagraph = -1
          }
        , Cmd.none
        )



---- UPDATE ----


type Msg
    = ParagraphClicked Int
    | SelectTool Tool
    | EditParagraph Int String
    | EditRaw String
    | StoreContent (List String)
    | HandleKeyboardEvent Tool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ParagraphClicked index ->
            let
                newContent =
                    model.content
                        |> List.indexedMap
                            (\i paragraph ->
                                if i == index then
                                    applyTool model.tool paragraph
                                else
                                    paragraph
                            )
            in
                ( { model | content = newContent, selectedParagraph = index }, debounceStoreContent newContent )

        SelectTool tool ->
            ( { model | tool = tool }, Cmd.none )

        EditParagraph index text ->
            let
                newContent =
                    List.indexedMap
                        (\i paragraph ->
                            if i == index then
                                text
                            else
                                paragraph
                        )
                        model.content
            in
                ( { model
                    | content = newContent
                  }
                , debounceStoreContent newContent
                )

        EditRaw text ->
            let
                newContent =
                    Regex.split Regex.All (Regex.regex "\n{2,}") text
            in
                ( { model | content = newContent }, debounceStoreContent newContent )

        StoreContent content ->
            if content == model.content then
                ( model, storeContent content )
            else
                -- The content has been updated, don't store this content (debounced)
                ( model, Cmd.none )

        HandleKeyboardEvent tool ->
            update (SelectTool tool) model


applyTool : Tool -> String -> String
applyTool tool paragraph =
    case tool of
        Title ->
            if String.startsWith "## " paragraph then
                String.dropLeft 3 paragraph
            else if String.startsWith "# " paragraph then
                "#" ++ paragraph
            else
                "# " ++ paragraph

        Quote ->
            let
                applyTransform : (String -> String) -> String
                applyTransform transform =
                    String.split "\n" paragraph
                        |> List.map transform
                        |> String.join "\n"
            in
                if String.startsWith "> " paragraph then
                    applyTransform (String.dropLeft 2)
                else
                    applyTransform ((++) "> ")

        Edit ->
            -- There's nothing to transform here, we're displaying textareas and reacting onInput.
            paragraph

        Raw ->
            -- There's nothing to transform here, we're displaying a textarea and reacting onInput.
            paragraph



---- VIEW ----


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ viewToolbar model.tool
        , Html.div
            [ Html.Attributes.class "main"
            ]
            [ (if model.tool == Raw then
                [ Html.textarea
                    [ Html.Attributes.class "raw"
                    , Html.Attributes.placeholder "No content yet, paste some raw text you'd like to format as markdown"
                    , Html.Events.onInput EditRaw
                    ]
                    [ Html.text (String.join "\n\n" model.content) ]
                ]
               else
                List.indexedMap
                    -- viewParagraph takes two more arguments: index and the
                    -- paragraph, that are passed by List.indexedMap
                    (viewParagraph model.tool model.selectedParagraph)
                    model.content
              )
                |> Html.div [ Html.Attributes.class "wrapper" ]
            ]
        ]


viewToolbar : Tool -> Html.Html Msg
viewToolbar selectedTool =
    let
        radio : Tool -> Html.Html Msg
        radio tool =
            Html.label []
                [ Html.input
                    [ Html.Attributes.type_ "radio"
                    , Html.Attributes.name "toolbar"
                    , Html.Attributes.checked (tool == selectedTool)
                    , Html.Events.onClick (SelectTool tool)
                    ]
                    []
                , Html.text (toString tool)
                ]
    in
        Html.div [ Html.Attributes.class "toolbar" ]
            [ Html.h1 []
                [ Html.text "Markdown Formatter" ]
            , Html.div [ Html.Attributes.class "tools" ]
                [ radio Raw
                , radio Title
                , radio Quote
                , radio Edit
                ]
            ]


viewParagraph : Tool -> Int -> Int -> String -> Html.Html Msg
viewParagraph tool selectedParagraph index paragraph =
    let
        classNames =
            if tool == Edit && selectedParagraph == index then
                "paragraph edit"
            else
                "paragraph display"
    in
        Html.div
            [ Html.Attributes.class classNames
            ]
            [ Html.textarea
                [ Html.Events.onInput (EditParagraph index)
                ]
                [ Html.text paragraph ]
            , Markdown.toHtml
                [ Html.Attributes.class "markdown"
                , Html.Events.onClick (ParagraphClicked index)
                ]
                paragraph
            ]



---- PROGRAM ----


debounceStoreContent : List String -> Cmd Msg
debounceStoreContent content =
    Process.sleep (Time.second)
        |> Task.andThen (always <| Task.succeed (StoreContent content))
        |> Task.perform identity


port storeContent : List String -> Cmd msg


keyboardShortcuts : Keyboard.Event.KeyboardEvent -> Maybe Msg
keyboardShortcuts { ctrlKey, keyCode } =
    if not ctrlKey then
        Nothing
    else
        case (toString keyCode) of
            "One" ->
                Just (HandleKeyboardEvent Raw)

            "Two" ->
                Just (HandleKeyboardEvent Title)

            "Three" ->
                Just (HandleKeyboardEvent Quote)

            "Four" ->
                Just (HandleKeyboardEvent Edit)

            _ ->
                Nothing


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.Events.onWindow "keydown" (Keyboard.Event.considerKeyboardEvent keyboardShortcuts)


main : Program StoredContent Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
