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
    = ToggleSelected String
    | SetTableState Table.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleSelected name ->
            ( { model | projects = List.map (toggle name) model.projects }
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )


toggle : String -> ( Bool, Project ) -> ( Bool, Project )
toggle name ( sel, p ) =
    if p.name == name then
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
            [ --checkboxColumn
              infoColumn

            -- , Table.stringColumn "Name" (Tuple.second >> .name)
            -- , Table.stringColumn "Link" (Tuple.second >> .link)
            , Table.stringColumn "Contributor Levels" (Tuple.second >> .contributorLevel)
            , Table.stringColumn "Contact" (Tuple.second >> .contact)

            -- , Table.stringColumn "Description" (Tuple.second >> .description)
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
            , onClick (ToggleSelected p.name)
            , HtmlA.class "clickable bookmark"
            ]
            []
        , a [ class "name", href p.link ] [ text p.name ]

        -- , Html.br [] []
        -- , text p.link
        , Html.br [] []
        , text p.description
        ]


simpleThead : List ( String, Table.Status, Attribute msg ) -> Table.HtmlDetails msg
simpleThead headers =
    Table.HtmlDetails [] (List.map simpleTheadHelp headers)


simpleTheadHelp : ( String, Table.Status, Attribute msg ) -> Html msg
simpleTheadHelp ( name, status, click ) =
    let
        title =
            Html.strong [] [ text name ]

        content =
            case status of
                Table.Unsortable ->
                    [ title ]

                Table.Sortable selected ->
                    [ if selected then
                        text "⇗ "

                      else
                        text "⇗ "
                    , title
                    ]

                Table.Reversible Nothing ->
                    [ text "⇕ "
                    , title
                    ]

                Table.Reversible (Just isReversed) ->
                    [ text
                        (if isReversed then
                            "⇘ "

                         else
                            "⇗ "
                        )
                    , title
                    ]
    in
    Html.th [ click, class "clickable" ] content
