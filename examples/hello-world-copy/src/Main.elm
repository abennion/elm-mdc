module Main exposing (..)

import Html exposing (Html, text)
import Json.Decode as Decode exposing (Value)
import Material
import Material.Button as Button
import Material.Options as Options exposing (styled)
import Navigation exposing (Location)
import Page
import Pages.Errored exposing (PageLoadError)
import Pages.Home
    exposing
        ( Model
        , Msg(Mdc)
        , defaultModel
        , update
        , view
        )
import Pages.Other
    exposing
        ( Model
        , Msg(Mdc)
        , defaultModel
        , update
        , view
        )
import Route exposing (Route(..))
import Task


type PageState
    = Loaded Route
    | TransitioningFrom Route


type alias Model =
    { mdc : Material.Model Msg
    , pageState : PageState
    , home : Pages.Home.Model Msg
    , other : Pages.Other.Model Msg
    }


defaultModel : Model
defaultModel =
    { mdc = Material.defaultModel
    , pageState = Loaded Route.Home
    , home = Pages.Home.defaultModel
    , other = Pages.Other.defaultModel
    }


type Msg
    = Mdc (Material.Msg Msg)
    | SetRoute (Maybe Route)
    | Click
    | HomeMsg (Pages.Home.Msg Msg)
    | OtherMsg (Pages.Other.Msg Msg)



-- | HomeLoaded (Result PageLoadError (Pages.Home.Model String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update msg" msg of
        Mdc msg_ ->
            Material.update Mdc msg_ model

        SetRoute route ->
            setRoute route model

        Click ->
            ( model, Cmd.none )

        -- ( HomeLoaded (Ok home), _ ) ->
        --     ( { model
        --         | home = home
        --         , pageState = Loaded Route.Home
        --       }
        --     , Cmd.none
        --     )
        -- ( HomeLoaded (Err error), _ ) ->
        --     ( { model | pageState = Loaded Route.Home }
        --     , Cmd.none
        --     )
        HomeMsg msg_ ->
            let
                ( home, effects ) =
                    Pages.Home.update HomeMsg msg_ model.home
            in
            ( { model | home = home }, effects )

        OtherMsg msg_ ->
            let
                ( other, effects ) =
                    Pages.Other.update OtherMsg msg_ model.home
            in
            ( { model | other = other }, effects )


getRoute : PageState -> Route
getRoute pageState =
    case pageState of
        Loaded route ->
            route

        TransitioningFrom route ->
            route


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        -- thinking about this all wrong. forget tasks and just come up with
        -- a could messages that we can use...
        transition toMsg task =
            ( { model | pageState = TransitioningFrom (getRoute model.pageState) }
            , Task.attempt toMsg task
            )
    in
    case Debug.log "setRoute maybeRoute" maybeRoute of
        Nothing ->
            ( model, Cmd.none )

        Just Route.Home ->
            -- transition HomeLoaded (Home.init Route.Home)
            ( { model | pageState = Loaded Route.Home }, Cmd.none )

        Just Route.Root ->
            ( { model | pageState = Loaded Route.Home }, Cmd.none )

        Just Route.Other ->
            ( { model | pageState = Loaded Route.Other }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.pageState of
        Loaded route ->
            viewPage model False route

        TransitioningFrom route ->
            viewPage model True route


viewPage : Model -> Bool -> Route -> Html Msg
viewPage model isLoading route =
    let
        page =
            { navigate = SetRoute
            , isLoading = False
            , body =
                \title nodes ->
                    styled Html.div
                        []
                        (List.concat
                            [ [ styled Html.h2 [] [ text title ]
                              ]
                            , nodes
                            ]
                        )
            }
    in
    case model.pageState of
        Loaded Route.Home ->
            Pages.Home.view HomeMsg page model.home

        Loaded Route.Other ->
            Pages.Other.view OtherMsg page model.other

        _ ->
            Html.div []
                [ Button.view Mdc
                    "my-button"
                    model.mdc
                    [ Button.ripple
                    , Options.onClick Click
                    ]
                    [ text "Click me!" ]
                ]


main : Program Value Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    let
        _ =
            Debug.log "init val" val

        _ =
            Debug.log "init location" location
    in
    setRoute (Route.fromLocation location)
        defaultModel


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model
