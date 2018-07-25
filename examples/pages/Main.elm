port module Main exposing (..)

import Html exposing (Html, text)
import Material
import Material.Options as Options exposing (css, styled, when)
import Material.Toolbar as Toolbar
import Material.Typography as Typography
import Navigation
import Pages.Home
import Pages.LayoutGrid
import Pages.Page as Page
import Pages.Theme
import Pages.Url as Url exposing (Url(..))
import Platform.Cmd exposing (..)


port scrollTop : () -> Cmd msg


type alias Model =
    { mdc : Material.Model Msg
    , url : Url
    , home : Pages.Home.Model Msg
    , layoutGrid : Pages.LayoutGrid.Model
    , theme : Pages.Theme.Model Msg
    }


defaultModel : Model
defaultModel =
    { mdc = Material.defaultModel
    , url = Home
    , home = Pages.Home.defaultModel
    , layoutGrid = Pages.LayoutGrid.defaultModel
    , theme = Pages.Theme.defaultModel
    }


type Msg
    = Mdc (Material.Msg Msg)
    | SetUrl Url
    | Navigate Url
    | HomeMsg (Pages.Home.Msg Msg)
    | LayoutGridMsg Pages.LayoutGrid.Msg
    | ThemeMsg (Pages.Theme.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdc msg ->
            Material.update Mdc msg model

        Navigate url ->
            { model | url = url }
                ! [ Navigation.newUrl (Url.toString url)
                  , scrollTop ()
                  ]

        SetUrl url ->
            { model | url = url }
                ! [ scrollTop ()
                  ]

        LayoutGridMsg msg_ ->
            let
                ( layoutGrid, effects ) =
                    Pages.LayoutGrid.update LayoutGridMsg msg_ model.layoutGrid
            in
            ( { model | layoutGrid = layoutGrid }, effects )

        HomeMsg msg_ ->
            let
                ( home, effects ) =
                    Pages.Home.update HomeMsg msg_ model.home
            in
            ( { model | home = home }, effects )

        ThemeMsg msg_ ->
            let
                ( theme, effects ) =
                    Pages.Theme.update ThemeMsg msg_ model.theme
            in
            ( { model | theme = theme }, effects )


view : Model -> Html Msg
view =
    view_



-- TODO: Should be: Html.Lazy.lazy view_, but triggers virtual-dom bug #110


view_ : Model -> Html Msg
view_ model =
    let
        page =
            { toolbar = Page.toolbar Mdc "page-toolbar" model.mdc Navigate model.url
            , fixedAdjust = Page.fixedAdjust "page-toolbar" model.mdc
            , navigate = Navigate
            , body =
                \title nodes ->
                    styled Html.div
                        [ css "display" "flex"
                        , css "flex-flow" "column"
                        , css "height" "100%"
                        , Typography.typography
                        ]
                        (List.concat
                            [ [ Page.toolbar Mdc "page-toolbar" model.mdc Navigate model.url title
                              ]
                            , [ styled Html.div [ Toolbar.fixedAdjust "page-toolbar" model.mdc ] []
                              ]
                            , nodes
                            ]
                        )
            }
    in
    case model.url of
        Home ->
            Pages.Home.view HomeMsg page model.home

        Theme ->
            Pages.Theme.view ThemeMsg page model.theme

        Error404 requestedHash ->
            Html.div
                []
                [ Options.styled Html.h1
                    [ Typography.display4
                    ]
                    [ text "404" ]
                , text requestedHash
                ]


urlOf : Model -> String
urlOf model =
    Url.toString model.url


main : Program Never Model Msg
main =
    Navigation.program
        (.hash >> Url.fromString >> SetUrl)
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        ( layoutGrid, layoutGridEffects ) =
            Pages.LayoutGrid.init LayoutGridMsg
    in
    ( { defaultModel
        | layoutGrid = layoutGrid
        , url = Url.fromString location.hash
      }
    , Cmd.batch
        [ Material.init Mdc
        , layoutGridEffects
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Material.subscriptions Mdc model
        , Pages.LayoutGrid.subscriptions LayoutGridMsg model.layoutGrid
        ]
