module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, div, text)
import Html.Attributes as HtmlA
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
              Table.stringColumn "Name" (Tuple.second >> .name)
            , Table.stringColumn "Link" (Tuple.second >> .link)
            , Table.stringColumn "Contributor Levels" (Tuple.second >> .contributorLevel)
            , Table.stringColumn "Rating" (Tuple.second >> .contact)
            , Table.stringColumn "Rating" (Tuple.second >> .description)
            ]
        , customizations =
            { defaultCustomizations | rowAttrs = toRowAttrs }
        }


toRowAttrs : ( Bool, Project ) -> List (Attribute Msg)
toRowAttrs ( sel, p ) =
    [ onClick (ToggleSelected p.name)
    , HtmlA.style "background"
        (if sel then
            "#CEFAF8"

         else
            "white"
        )
    ]
