module Main exposing (..)

import Html exposing (Html, text)
import Json.Decode as Decode exposing (Value)
import Material
import Material.Button as Button
import Material.LinearProgress as LinearProgress
import Material.Options as Options exposing (cs, css, styled, when)
import Material.Typography
import Navigation
import Page.Buttons
import Page.Chips
import Page.LayoutGrid
import Page.Page as Page
import Ports exposing (scrollTop)
import Url as Url exposing (ToolbarPage(..), TopAppBarPage(..), Url(..))


-- MAIN


main : Program (Maybe Value) Model Msg
main =
    Navigation.programWithFlags
        (.hash >> Url.fromString >> SetUrl)
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }



-- MODEL


type alias Model =
    { mdc : Material.Model Msg
    , url : Url
    , pageState : PageState
    , buttons : Page.Buttons.Model Msg
    , chips : Page.Chips.Model Msg
    , layoutGrid : Page.LayoutGrid.Model
    }


defaultModel : Model
defaultModel =
    { mdc = Material.defaultModel
    , url = Button
    , pageState = Loaded Button
    , buttons = Page.Buttons.defaultModel
    , chips = Page.Chips.defaultModel
    , layoutGrid = Page.LayoutGrid.defaultModel
    }


type Msg
    = Mdc (Material.Msg Msg)
    | SetUrl Url
    | Navigate Url
    | Click
    | ButtonsMsg (Page.Buttons.Msg Msg)
    | ChipsMsg (Page.Chips.Msg Msg)
    | LayoutGridMsg (Page.LayoutGrid.Msg Msg)


type PageState
    = Loaded Url
    | TransitioningFrom Url



-- INIT


init : Maybe Value -> Navigation.Location -> ( Model, Cmd Msg )
init val location =
    ( defaultModel, Material.init Mdc )



-- UPDATE
-- setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
-- setRoute maybeRoute model =
--     let
--         transition toMsg task =
--             { model | pageState = TransitioningFrom (getPage model.pageState) }
--                 => Task.attempt toMsg task
--         errored =
--             pageErrored model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "Main.update.msg" msg of
        Mdc msg_ ->
            Material.update Mdc msg_ model

        Navigate url ->
            { model | url = url, pageState = Loaded url }
                ! [ Navigation.newUrl (Url.toString url)
                  , scrollTop ()
                  ]

        SetUrl url ->
            { model | url = url }
                ! [ scrollTop ()
                  ]

        ButtonsMsg msg_ ->
            ( model, Cmd.none )

        ChipsMsg msg_ ->
            ( model, Cmd.none )

        LayoutGridMsg msg_ ->
            ( model, Cmd.none )

        Click ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case Debug.log "view model.pageState" model.pageState of
        Loaded url ->
            viewPage model True url

        TransitioningFrom url ->
            viewPage model False url


viewPage : Model -> Bool -> Url -> Html Msg
viewPage model isLoading url =
    let
        page =
            { toolbar = Page.toolbar Mdc "page-toolbar" model.mdc Navigate model.url
            , fixedAdjust = Page.fixedAdjust "page-toolbar" model.mdc
            , navigate = Navigate
            , body =
                \title nodes ->
                    styled Html.div
                        []
                        [ Html.h2 [] [ text title ]
                        , styled Html.div
                            [ css "margin" "24px"
                            , css "margin-top" "0"
                            , css "margin-bottom" "16px"
                            ]
                            [ LinearProgress.view
                                [ LinearProgress.buffered 0.3 0.0
                                , LinearProgress.indeterminate
                                , cs "demo-linear-progress--custom"
                                ]
                                []
                            ]
                        , Button.view Mdc
                            "my-button"
                            model.mdc
                            [ Button.ripple
                            , Options.onClick (Navigate Chips)
                            ]
                            [ text "Click me!" ]
                        ]
            }
    in
    case Debug.log "viewPage url" url of
        Button ->
            Page.Buttons.view ButtonsMsg page model.buttons

        Chips ->
            Page.Chips.view ChipsMsg page model.chips

        Error404 msg ->
            Page.Buttons.view ButtonsMsg page model.buttons



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model
