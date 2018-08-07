module Pages.Home exposing (Model, Msg(Mdc), defaultModel, update, view)

import Html exposing (Html, div, text)
import Material
import Material.Button as Button
import Material.LinearProgress as LinearProgress
import Material.Options as Options exposing (cs, css, styled, when)
import Material.Tabs as TabBar
import Page exposing (Page)
import Pages.Errored exposing (PageLoadError, pageLoadError)
import Process
import Route exposing (Route)
import Task exposing (Task)
import Time


type Tab
    = Cats
    | Dogs


type TabState
    = TransitioningFrom Tab
    | Loaded Tab


type alias ErrorMsg =
    String


type alias Model m =
    { mdc : Material.Model m
    , text : String
    , tabState : TabState
    }


defaultModel : Model m
defaultModel =
    { mdc = Material.defaultModel
    , text = "Nothing to see here."
    , tabState = Loaded Cats
    }


type Msg m
    = Mdc (Material.Msg m)
    | Click String
    | SelectTab (Maybe Tab)
    | CatsLoaded (Result ErrorMsg (Model m))
    | DogsLoaded (Result ErrorMsg (Model m))


getTab : TabState -> Tab
getTab tabState =
    case tabState of
        Loaded tab ->
            tab

        TransitioningFrom tab ->
            tab


delay : Time.Time -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


setTab : (Msg m -> m) -> Maybe Tab -> Model m -> ( Model m, Cmd m )
setTab lift maybeTab model =
    let
        transition toMsg task =
            ( { model | tabState = TransitioningFrom (getTab model.tabState) }
            , Task.attempt toMsg task
            )
    in
    case Debug.log "setTab maybeTab" maybeTab of
        Nothing ->
            ( model, Cmd.none )

        Just Cats ->
            transition (lift << CatsLoaded)
                (Task.map
                    (\_ -> model)
                    (Process.sleep (Time.second * 5))
                )

        Just Dogs ->
            transition (lift << DogsLoaded)
                (Task.map
                    (\_ -> model)
                    (Process.sleep (Time.second * 5))
                )


update : (Msg m -> m) -> Msg m -> Model m -> ( Model m, Cmd m )
update lift msg model =
    case Debug.log "Home.update msg" msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model

        Click text ->
            ( { model | text = text }
            , Cmd.none
            )

        SelectTab maybeTab ->
            setTab lift maybeTab model

        CatsLoaded (Ok home) ->
            ( { model
                | tabState = Loaded Cats
              }
            , Cmd.none
            )

        CatsLoaded (Err error) ->
            ( model
            , Cmd.none
            )

        DogsLoaded (Ok home) ->
            ( { model
                | tabState = Loaded Dogs
              }
            , Cmd.none
            )

        DogsLoaded (Err error) ->
            ( model
            , Cmd.none
            )


view : (Msg m -> m) -> Page m -> Model m -> Html m
view lift page model =
    case Debug.log "view model.tabState" model.tabState of
        Loaded tab ->
            viewPage lift page model False tab

        TransitioningFrom tab ->
            viewPage lift page model True tab


viewPage : (Msg m -> m) -> Page m -> Model m -> Bool -> Tab -> Html m
viewPage lift page model isLoading tab =
    let
        spinner isLoading =
            case isLoading of
                True ->
                    LinearProgress.view
                        [ LinearProgress.buffered 0.0 0.0
                        , LinearProgress.indeterminate
                        , cs "demo-linear-progress--custom"
                        ]
                        []

                False ->
                    Html.text ""
    in
    page.body "Home"
        page.isLoading
        [ styled Html.div
            []
            [ TabBar.view (lift << Mdc)
                "my-tab-bar"
                model.mdc
                [ TabBar.indicator
                , TabBar.scrolling
                ]
                [ TabBar.tab
                    [ Options.onClick (lift (SelectTab (Just Cats)))
                    ]
                    [ text "Item One" ]
                , TabBar.tab
                    [ Options.onClick (lift (SelectTab (Just Dogs)))
                    ]
                    [ text "Item Two" ]
                , TabBar.tab [] [ text "Item Three" ]
                , TabBar.tab [] [ text "Item Four" ]
                , TabBar.tab [] [ text "Item Five" ]
                , TabBar.tab [] [ text "Item Six" ]
                , TabBar.tab [] [ text "Item Seven" ]
                , TabBar.tab [] [ text "Item Eight" ]
                , TabBar.tab [] [ text "Item Nine" ]
                ]
            , spinner isLoading
            , styled Html.h2
                []
                [ text model.text
                ]
            , styled Html.h2
                []
                [ text ("Is loading: " ++ toString page.isLoading)
                ]
            , styled Html.h2
                []
                [ case getTab model.tabState of
                    Cats ->
                        text "Cats"

                    Dogs ->
                        text "Dogs"
                ]
            , Button.view (lift << Mdc)
                "my-button"
                model.mdc
                [ Button.ripple
                , Options.onClick (page.navigate (Just Route.Other))
                ]
                [ text "Other!"
                ]
            , styled Html.div
                [ css "padding" "24px" ]
                [ text "nothing here"
                ]
            , styled Html.div
                [ css "padding" "24px" ]
                [ text "nothing here"
                ]
            , styled Html.div
                [ css "padding" "24px" ]
                [ text "nothing here"
                ]
            , styled Html.div
                [ css "padding" "24px" ]
                [ text "nothing here"
                ]
            , styled Html.div
                [ css "padding" "24px" ]
                [ text "nothing here"
                ]
            , styled Html.div
                [ css "padding" "24px" ]
                [ text "nothing here"
                ]
            , styled Html.div
                [ css "padding" "24px" ]
                [ text "nothing here"
                ]
            , styled Html.div
                [ css "padding" "24px" ]
                [ text "nothing here"
                ]
            , styled Html.div
                [ css "padding" "24px" ]
                [ text "nothing here"
                ]
            , styled Html.div
                [ css "padding" "24px" ]
                [ text "nothing here"
                ]
            , styled Html.div
                [ css "padding" "24px" ]
                [ text "nothing here"
                ]
            , styled Html.div
                [ css "padding" "24px" ]
                [ text "nothing here"
                ]
            ]
        ]