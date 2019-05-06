module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, a, div, text)
import Html.Attributes as HtmlA exposing (class, href)
import Html.Events exposing (onClick)
import Projects exposing (..)
import Set exposing (Set)
import Table exposing (defaultCustomizations)


main =
    Browser.element
        { init = \() -> init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { projects : List Project
    , selectedProjects : Set Int
    , tableState : Table.State
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { projects = projects
            , selectedProjects = Set.empty
            , tableState = Table.initialSort "Name"
            }
    in
    ( model, Cmd.none )


type Msg
    = ToggleSelected Int
    | SetTableState Table.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ selectedProjects } as model) =
    case msg of
        ToggleSelected id ->
            let
                newSelectedProjects =
                    if Set.member id selectedProjects then
                        Set.remove id selectedProjects

                    else
                        Set.insert id selectedProjects
            in
            ( { model | selectedProjects = newSelectedProjects }
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )


view : Model -> Html Msg
view { projects, tableState, selectedProjects } =
    div []
        [ Html.h1 [] [ text "Projects" ]
        , Table.view (tableConfig selectedProjects) tableState projects
        ]


tableConfig : Set Int -> Table.Config Project Msg
tableConfig selectedIds =
    Table.customConfig
        { toId = .name
        , toMsg = SetTableState
        , columns =
            [ infoColumn selectedIds
            , Table.stringColumn "Contact" .contact
            , Table.stringColumn "Level" .contributorLevel
            ]
        , customizations =
            { defaultCustomizations
                | tableAttrs = [ class "projects" ]
                , thead = simpleThead
                , rowAttrs = toRowAttrs selectedIds
            }
        }


toRowAttrs : Set Int -> Project -> List (Attribute Msg)
toRowAttrs selectedIds p =
    [ class
        (if Set.member p.id selectedIds then
            "selected"

         else
            ""
        )
    ]


infoColumn : Set Int -> Table.Column Project Msg
infoColumn selectedIds =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = viewInfo selectedIds
        , sorter = Table.increasingOrDecreasingBy .name
        }


viewInfo : Set Int -> Project -> Table.HtmlDetails Msg
viewInfo selectedIds p =
    let
        iconFile =
            if Set.member p.id selectedIds then
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
