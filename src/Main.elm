module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, a, div, text)
import Html.Attributes as HtmlA exposing (class, href)
import Html.Events exposing (onClick)
import Projects exposing (..)
import Table exposing (defaultCustomizations)


main =
    Browser.element
        { init = \() -> init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { projects : List ( Bool, Project )
    , tableState : Table.State
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { projects = List.map (\p -> ( False, p )) projects
            , tableState = Table.initialSort "Name"
            }
    in
    ( model, Cmd.none )


type Msg
    = ToggleSelected Int
    | SetTableState Table.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleSelected id ->
            ( { model | projects = List.map (toggle id) model.projects }
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )


toggle : Int -> ( Bool, Project ) -> ( Bool, Project )
toggle id ( sel, p ) =
    if p.id == id then
        ( not sel, p )

    else
        ( sel, p )


view : Model -> Html Msg
view { projects, tableState } =
    div []
        [ Html.h1 [] [ text "Projects" ]
        , Table.view tableConfig tableState projects
        ]


tableConfig : Table.Config ( Bool, Project ) Msg
tableConfig =
    Table.customConfig
        { toId = Tuple.second >> .name
        , toMsg = SetTableState
        , columns =
            [ infoColumn
            , Table.stringColumn "Level" (Tuple.second >> .contributorLevel)
            , Table.stringColumn "Contact" (Tuple.second >> .contact)
            ]
        , customizations =
            { defaultCustomizations
                | tableAttrs = [ class "projects" ]
                , thead = simpleThead
                , rowAttrs = toRowAttrs
            }
        }


toRowAttrs : ( Bool, Project ) -> List (Attribute Msg)
toRowAttrs ( sel, p ) =
    [ class
        (if sel then
            "selected"

         else
            ""
        )
    ]


infoColumn : Table.Column ( Bool, Project ) Msg
infoColumn =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = viewInfo
        , sorter = Table.increasingOrDecreasingBy (Tuple.second >> .name)
        }


viewInfo : ( Bool, Project ) -> Table.HtmlDetails Msg
viewInfo ( sel, p ) =
    let
        iconFile =
            if sel then
                "bookmark-solid.svg"

            else
                "bookmark-regular.svg"
    in
    Table.HtmlDetails
        []
        [ Html.img
            [ HtmlA.src iconFile
            , onClick (ToggleSelected p.id)
            , HtmlA.class "clickable bookmark"
            ]
            []
        , a [ class "name", href p.link ] [ text p.name ]

        -- , Html.br [] []
        -- , text p.link
        , Html.p [] [ text p.description ]
        ]


simpleThead : List ( String, Table.Status, Attribute msg ) -> Table.HtmlDetails msg
simpleThead headers =
    Table.HtmlDetails [] (List.map simpleTheadHelp headers)


simpleTheadHelp : ( String, Table.Status, Attribute msg ) -> Html msg
simpleTheadHelp ( name, status, click ) =
    let
        title =
            text name

        content =
            case status of
                Table.Unsortable ->
                    [ title ]

                Table.Sortable selected ->
                    [ title
                    , if selected then
                        text " ⇗ "

                      else
                        text " ⇗ "
                    ]

                Table.Reversible Nothing ->
                    [ title
                    , text " ⇕ "
                    ]

                Table.Reversible (Just isReversed) ->
                    [ title
                    , text
                        (if isReversed then
                            " ⇘ "

                         else
                            " ⇗ "
                        )
                    ]
    in
    Html.th [ click, class "clickable" ] content
