port module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, a, div, text)
import Html.Attributes as HtmlA exposing (class, href)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as P
import Json.Encode as Encode
import Set exposing (Set)
import Table exposing (defaultCustomizations)


port save : Value -> Cmd msg


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { projects : List Project
    , selectedProjects : Set Int
    , tableState : Table.State
    }


type alias Project =
    { id : Int
    , name : String
    , link : String
    , contributorLevel : String
    , contact : String
    , description : String
    }


readProjects : Cmd Msg
readProjects =
    Http.get
        { url = "./projects.json"
        , expect = Http.expectJson GotProjects (Decode.list projectDecoder)
        }


projectDecoder : Decoder Project
projectDecoder =
    Decode.succeed Project
        |> P.required "id" Decode.int
        |> P.required "name" Decode.string
        |> P.required "link" Decode.string
        |> P.required "contributor level" Decode.string
        |> P.required "contact" Decode.string
        |> P.required "description" Decode.string


idsDecoder : Decoder (List Int)
idsDecoder =
    Decode.list Decode.int


init : List Int -> ( Model, Cmd Msg )
init selectedProjects =
    let
        model =
            { projects = []
            , selectedProjects = Set.fromList selectedProjects
            , tableState = Table.initialSort "Name"
            }
    in
    ( model, readProjects )


type Msg
    = ToggleSelected Int
    | SetTableState Table.State
    | GotProjects (Result Http.Error (List Project))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ selectedProjects } as model) =
    case msg of
        GotProjects (Ok projects) ->
            ( { model | projects = projects }
            , Cmd.none
            )

        GotProjects _ ->
            ( model, Cmd.none )

        ToggleSelected id ->
            let
                newSelectedProjects =
                    if Set.member id selectedProjects then
                        Set.remove id selectedProjects

                    else
                        Set.insert id selectedProjects

                cmd =
                    Set.toList newSelectedProjects
                        |> Encode.list Encode.int
                        |> save
            in
            ( { model | selectedProjects = newSelectedProjects }
            , cmd
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
