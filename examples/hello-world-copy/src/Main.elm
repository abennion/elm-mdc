module Main exposing (..)

import Html exposing (Html, text)
import Json.Decode as Decode exposing (Value)
import Material
import Material.Button as Button
import Material.Options as Options
import Navigation exposing (Location)
import Pages.Home
    exposing
        ( Model
        , Msg(Mdc)
        , defaultModel
        , update
        , view
        )
import Route exposing (Route(..))


type alias Model =
    { mdc : Material.Model Msg
    , home : Pages.Home.Model Msg
    }


defaultModel : Model
defaultModel =
    { mdc = Material.defaultModel
    , home = Pages.Home.defaultModel
    }


type Msg
    = Mdc (Material.Msg Msg)
    | SetRoute (Maybe Route)
    | Click
    | HomeMsg (Pages.Home.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update msg" msg of
        Mdc msg_ ->
            Material.update Mdc msg_ model

        SetRoute route ->
            setRoute route model

        Click ->
            ( model, Cmd.none )

        HomeMsg msg_ ->
            let
                ( home, effects ) =
                    Pages.Home.update HomeMsg msg_ model.home
            in
            ( { model | home = home }, effects )


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case Debug.log "setRoute maybeRoute" maybeRoute of
        Nothing ->
            ( model, Cmd.none )

        Just Route.Home ->
            ( model, Route.modifyUrl Route.Home )

        Just Route.Root ->
            ( model, Cmd.none )

        Just Route.Other ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    -- put Pages.Home.view in here
    -- need to pass the page or url we're on...
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
init value location =
    ( defaultModel, Material.init Mdc )


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model
