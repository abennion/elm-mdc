module Pages.Other exposing (Model, Msg(Mdc), defaultModel, update, view)

import Html exposing (Html, div, text)
import Material
import Material.Button as Button
import Material.LinearProgress as LinearProgress
import Material.Options as Options exposing (cs, css, styled, when)
import Route exposing (Route)
import Views.Page exposing (Page)


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
    let
        fakeText =
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do"
                ++ " eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut"
                ++ " enim ad minim veniam, quis nostrud exercitation ullamco laboris"
                ++ " nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in"
                ++ " reprehenderit in voluptate velit esse cillum dolore eu fugiat"
                ++ " nulla pariatur. Excepteur sint occaecat cupidatat non proident,"
                ++ " sunt in culpa qui officia deserunt mollit anim id est laborum."
    in
    page.body "Other"
        page.isLoading
        [ styled Html.div
            []
            [ styled Html.h2
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
                , Options.onClick (page.setRoute (Just Route.Home))
                ]
                [ text "Home!"
                ]
            , styled Html.div
                [ css "padding" "24px"
                ]
                [ styled Html.p
                    [ css "padding" "8px" ]
                    [ text fakeText ]
                ]
            , styled Html.div
                [ css "padding" "24px"
                ]
                [ styled Html.p
                    [ css "padding" "8px" ]
                    [ text fakeText ]
                ]
            , styled Html.div
                [ css "padding" "24px"
                ]
                [ styled Html.p
                    [ css "padding" "8px" ]
                    [ text fakeText ]
                ]
            , styled Html.div
                [ css "padding" "24px"
                ]
                [ styled Html.p
                    [ css "padding" "8px" ]
                    [ text fakeText ]
                ]
            , styled Html.div
                [ css "padding" "24px"
                ]
                [ styled Html.p
                    [ css "padding" "8px" ]
                    [ text fakeText ]
                ]
            , styled Html.div
                [ css "padding" "24px"
                ]
                [ styled Html.p
                    [ css "padding" "8px" ]
                    [ text fakeText ]
                ]
            , styled Html.div
                [ css "padding" "24px"
                ]
                [ styled Html.p
                    [ css "padding" "8px" ]
                    [ text fakeText ]
                ]
            , styled Html.div
                [ css "padding" "24px"
                ]
                [ styled Html.p
                    [ css "padding" "8px" ]
                    [ text fakeText ]
                ]
            , styled Html.div
                [ css "padding" "24px"
                ]
                [ styled Html.p
                    [ css "padding" "8px" ]
                    [ text fakeText ]
                ]
            , styled Html.div
                [ css "padding" "24px"
                ]
                [ styled Html.p
                    [ css "padding" "8px" ]
                    [ text fakeText ]
                ]
            , styled Html.div
                [ css "padding" "24px"
                ]
                [ styled Html.p
                    [ css "padding" "8px" ]
                    [ text fakeText ]
                ]
            ]
        ]
