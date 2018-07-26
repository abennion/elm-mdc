port module Pages.Todo exposing (Model, Msg(Mdc), defaultModel, subscriptions, update, view)

import Data.User as User exposing (User, decoder)
import Html exposing (Html, text)
import Json.Decode as Decode exposing (Value)
import Material
import Material.Button as Button
import Material.Options as Options exposing (cs, css, onClick, styled, when)
import Material.Textfield as Textfield
import Material.Theme as Theme
import Material.Typography as Typography
import Pages.Page as Page exposing (Page)
import Ports
import Request.User exposing (storeSession)


type alias Model m =
    { mdc : Material.Model m
    , message : String
    , email : String
    }


defaultModel : Model m
defaultModel =
    { mdc = Material.defaultModel
    , message = ""
    , email = ""
    }


type Msg m
    = Mdc (Material.Msg m)
    | UpdateMessage String
    | SaveEmail String
    | UpdateEmail (Maybe User)


update : (Msg m -> m) -> Msg m -> Model m -> ( Model m, Cmd m )
update lift msg model =
    case Debug.log "Todo.update.msg:" msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model

        UpdateMessage msg_ ->
            { model | message = msg_ } ! []

        SaveEmail msg_ ->
            ( model
            , storeSession
                (User model.message)
            )

        UpdateEmail msg_ ->
            let
                _ =
                    Debug.log "UpdateEmail" msg_

                userText =
                    case msg_ of
                        Just user ->
                            user.email

                        Nothing ->
                            "not happenin'"
            in
            { model | email = userText } ! []



-- StoreText msg_ ->


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
                        (Button.onClick (lift (SaveEmail model.message))
                            :: options
                        )
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
    page.body "Todo"
        [ styled Html.div
            [ css "padding" "24px"
            , Theme.secondary
            , Theme.secondaryBg
            , Theme.textSecondaryOnLight
            ]
            [ text model.email
            ]
        , styled Html.div
            []
            [ Textfield.view (lift << Mdc)
                "my-text-field"
                model.mdc
                [ Textfield.label "Text field"
                , Options.onInput (lift << UpdateMessage)
                , Textfield.fullwidth
                , css "padding" "16px"
                , css "margin" "8px"
                , css "background-color" "rgba(255, 255, 255, 0.1)"
                ]
                []
            ]
        , styled Html.div
            []
            [ Button.view (lift << Mdc)
                ("save-cookie" ++ "-baseline-button")
                model.mdc
                [ Button.onClick (lift (SaveEmail model.message))
                , Button.raised
                , Button.ripple
                , css "margin" "16px"
                ]
                [ text "Save Cookie" ]
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


subscriptions : (Msg m -> m) -> Model m -> Sub m
subscriptions lift model =
    let
        _ =
            Debug.log "Todo.subscriptions" model
    in
    Sub.map (lift << UpdateEmail) sessionChange


sessionChange : Sub (Maybe User)
sessionChange =
    let
        _ =
            Debug.log "sessionChange" Ports.onSessionChange
    in
    Ports.onSessionChange (Decode.decodeValue User.decoder >> Result.toMaybe)
