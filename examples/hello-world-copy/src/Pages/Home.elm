module Pages.Home exposing (Model, Msg(Mdc), defaultModel, update, view)

import Html exposing (Html, div, text)
import Material
import Material.Button as Button
import Material.LinearProgress as LinearProgress
import Material.Options as Options exposing (cs, css, styled, when)
import Material.Tabs as TabBar
import Page exposing (Page)
import Pages.Errored exposing (PageLoadError, pageLoadError)
import Route exposing (Route)
import Task exposing (Task)


type alias Model m =
    { mdc : Material.Model m
    , text : String
    }


defaultModel : Model m
defaultModel =
    { mdc = Material.defaultModel
    , text = "Nothing to see here."
    }


type Msg m
    = Mdc (Material.Msg m)
    | Click String


update : (Msg m -> m) -> Msg m -> Model m -> ( Model m, Cmd m )
update lift msg model =
    case msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model

        Click text ->
            ( { model | text = text }
            , Cmd.none
            )


view : (Msg m -> m) -> Page m -> Model m -> Html m
view lift page model =
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
                [ TabBar.tab [] [ text "Item One" ]
                , TabBar.tab [] [ text "Item Two" ]
                , TabBar.tab [] [ text "Item Three" ]
                , TabBar.tab [] [ text "Item Four" ]
                , TabBar.tab [] [ text "Item Five" ]
                , TabBar.tab [] [ text "Item Six" ]
                , TabBar.tab [] [ text "Item Seven" ]
                , TabBar.tab [] [ text "Item Eight" ]
                , TabBar.tab [] [ text "Item Nine" ]
                ]
            , styled Html.h2
                []
                [ text model.text
                ]
            , styled Html.h2
                []
                [ text ("Is loading: " ++ toString page.isLoading)
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
