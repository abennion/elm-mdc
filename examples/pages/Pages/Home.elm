module Pages.Home exposing (Model, Msg(Mdc), defaultModel, update, view)

import Html exposing (Html, text)
import Html.Events exposing (onInput)
import Material
import Material.Button as Button
import Material.Options as Options exposing (cs, css, styled, when)
import Material.Textfield as Textfield
import Material.Typography as Typography
import Pages.Page as Page exposing (Page)


type alias Model m =
    { mdc : Material.Model m
    , message : String
    }


defaultModel : Model m
defaultModel =
    { mdc = Material.defaultModel
    , message = ""
    }


type Msg m
    = Mdc (Material.Msg m)
    | UpdateTextMsg String


update : (Msg m -> m) -> Msg m -> Model m -> ( Model m, Cmd m )
update lift msg model =
    case msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model

        UpdateTextMsg msg_ ->
            { model | message = msg_ } ! []


view : (Msg m -> m) -> Page m -> Model m -> Html m
view lift page model =
    let
        textButtons idx =
            example idx
                "Text Button"
                [ Button.ripple
                , css "margin" "16px"
                ]

        raisedButtons idx =
            example idx
                "Raised Button"
                [ Button.raised
                , Button.ripple
                , css "margin" "16px"
                ]

        unelevatedButtons idx =
            example idx
                "Unelevated Button"
                [ Button.unelevated
                , Button.ripple
                , css "margin" "16px"
                ]

        outlinedButtons idx =
            example idx
                "Outlined Button"
                [ Button.outlined
                , Button.ripple
                , css "margin" "16px"
                ]

        example idx title options =
            styled Html.div
                [ css "padding" "0 24px 16px"
                ]
                [ styled Html.div
                    [ Typography.title
                    , css "padding" "48px 16px 24px"
                    ]
                    [ text title
                    ]
                , styled Html.div
                    []
                    [ Button.view (lift << Mdc)
                        (idx ++ "-baseline-button")
                        model.mdc
                        options
                        [ text "Baseline" ]
                    , Button.view (lift << Mdc)
                        (idx ++ "-dense-button")
                        model.mdc
                        (Button.dense
                            :: options
                        )
                        [ text "Dense" ]
                    , Button.view (lift << Mdc)
                        (idx ++ "-secondary-button")
                        model.mdc
                        (cs "secondary-button"
                            :: options
                        )
                        [ text "Secondary" ]
                    , Button.view (lift << Mdc)
                        (idx ++ "-icon-button")
                        model.mdc
                        (Button.icon "favorite"
                            :: options
                        )
                        [ text "Icon" ]
                    , Button.view (lift << Mdc)
                        (idx ++ "-link-button")
                        model.mdc
                        (Button.link "#theme"
                            :: options
                        )
                        [ text "Link Theme" ]
                    ]
                ]
    in
    page.body "Home"
        [ styled Html.div
            [ css "padding" "24px" ]
            [ text model.message
            ]
        , styled Html.div
            [ css "padding" "24px" ]
            [ Textfield.view (lift << Mdc)
                "my-text-field"
                model.mdc
                [ Textfield.label "Text field"
                , Options.onInput (lift << UpdateTextMsg)
                ]
                []
            ]
        , styled Html.div
            [ cs "demo-wrapper"
            ]
            [ styled Html.h1
                [ Typography.display2
                , css "padding-left" "36px"
                , css "padding-top" "64px"
                , css "padding-bottom" "8px"
                ]
                [ text "Ripple Enabled"
                ]
            , textButtons "buttons-text-buttons"
            , raisedButtons "buttons-raised-buttons"
            , unelevatedButtons "buttons-unelevated-buttons"
            , outlinedButtons "buttons-outlined-buttons"
            ]
        ]
