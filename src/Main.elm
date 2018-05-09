port module Main exposing (..)

import Html
import Html.Attributes
import Html.Events
import Markdown


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
                ( { model | content = newContent, selectedParagraph = index }, storeContent newContent )

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
                , storeContent newContent
                )

        EditRaw text ->
            let
                newContent =
                    String.split "\n\n" text
            in
                ( { model | content = newContent }, storeContent newContent )


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
        , Html.div [ Html.Attributes.class "main" ]
            [ Html.h1 []
                [ Html.text "Markdown Formatter" ]
            , (if model.tool == Raw then
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
            [ radio Raw
            , radio Title
            , radio Quote
            , radio Edit
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


port storeContent : List String -> Cmd msg


main : Program StoredContent Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
