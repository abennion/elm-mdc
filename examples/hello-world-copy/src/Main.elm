module Main exposing (..)

-- import Pages.Errored exposing (PageLoadError)

import Html exposing (Html, text)
import Json.Decode as Decode exposing (Value)
import Material
import Material.Button as Button
import Material.LinearProgress as LinearProgress
import Material.Options as Options exposing (cs, css, styled)
import Material.Typography as Typography
import Navigation exposing (Location)
import Page exposing (Page)
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
import Process
import Route exposing (Route(..))
import Task
import Time


type PageState
    = Loaded Route
    | TransitioningFrom Route


type alias ErrorMsg =
    String


type alias Model =
    { mdc : Material.Model Msg
    , pageState : PageState
    , home : Pages.Home.Model Msg
    , other : Pages.Other.Model Msg
    , error : ErrorMsg
    , page : Page.Model Msg
    }


defaultModel : Model
defaultModel =
    { mdc = Material.defaultModel
    , pageState = Loaded Route.Home
    , home = Pages.Home.defaultModel
    , other = Pages.Other.defaultModel
    , error = "nothing"
    , page = Page.defaultModel
    }


type Msg
    = Mdc (Material.Msg Msg)
    | SetRoute (Maybe Route)
    | Click
    | HomeMsg (Pages.Home.Msg Msg)
    | OtherMsg (Pages.Other.Msg Msg)
    | HomeLoaded (Result ErrorMsg (Pages.Home.Model Msg))
    | OtherLoaded (Result ErrorMsg (Pages.Other.Model Msg))
    | PageMsg (Page.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update msg" msg of
        Mdc msg_ ->
            Material.update Mdc msg_ model

        SetRoute route ->
            setRoute route model

        Click ->
            ( model, Cmd.none )

        PageMsg msg_ ->
            let
                ( page, effects ) =
                    Page.update PageMsg msg_ model.page
            in
            ( { model | page = page }, effects )

        HomeLoaded (Ok home) ->
            ( { model
                | error = ""
                , pageState = Loaded Route.Home
              }
            , Cmd.none
            )

        HomeLoaded (Err error) ->
            ( { model
                | error = error
                , pageState = Loaded Route.Home
              }
            , Cmd.none
            )

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

        OtherLoaded (Ok home) ->
            ( { model
                | error = ""
                , pageState = Loaded Route.Other
              }
            , Cmd.none
            )

        OtherLoaded (Err error) ->
            ( { model
                | error = error
                , pageState = Loaded Route.Other
              }
            , Cmd.none
            )


getRoute : PageState -> Route
getRoute pageState =
    case pageState of
        Loaded route ->
            route

        TransitioningFrom route ->
            route


delay : Time.Time -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        transition toMsg task =
            ( { model | pageState = TransitioningFrom (getRoute model.pageState) }
            , Task.attempt toMsg task
            )
    in
    case Debug.log "setRoute maybeRoute" maybeRoute of
        Nothing ->
            ( model, Cmd.none )

        Just Route.Home ->
            transition HomeLoaded
                (Task.map
                    (\_ -> model.home)
                    (Process.sleep (Time.second * 5))
                )

        Just Route.Root ->
            ( { model | pageState = Loaded Route.Home }, Cmd.none )

        Just Route.Other ->
            transition OtherLoaded
                (Task.map
                    (\_ -> model.other)
                    (Process.sleep (Time.second * 5))
                )


view : Model -> Html Msg
view model =
    case Debug.log "view model.pageState" model.pageState of
        Loaded route ->
            viewPage model False route

        TransitioningFrom route ->
            viewPage model True route


viewPage : Model -> Bool -> Route -> Html Msg
viewPage model isLoading route =
    let
        spinner isLoading =
            case page.isLoading of
                True ->
                    styled Html.div
                        [ cs "mdc-theme--dark-background"
                        , css "height" "8px"
                        ]
                        [ LinearProgress.view
                            [ LinearProgress.buffered 0.0 0.0
                            , cs "demo-linear-progress--custom"
                            ]
                            []
                        ]

                False ->
                    styled Html.div
                        [ cs "mdc-theme--dark-background"
                        , css "height" "8px"
                        ]
                        [ text ""
                        ]

        page =
            { navigate = SetRoute
            , isLoading = isLoading
            , body =
                \title nodes ->
                    styled Html.div
                        [ css "display" "flex"
                        , css "flex-flow" "column"
                        , css "height" "100%"
                        , Typography.typography
                        ]
                        (List.concat
                            [ [ spinner isLoading
                              , Page.drawer PageMsg model.page
                              , Page.toolbar
                                    PageMsg
                                    model.page
                                    SetRoute
                                    (getRoute model.pageState)
                                    title
                                    "spam281@gmail.com"
                              ]
                            , nodes
                            ]
                        )
            }
    in
    case getRoute model.pageState of
        Route.Home ->
            Pages.Home.view HomeMsg page model.home

        Route.Other ->
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
