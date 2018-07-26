port module Pages.Todo exposing (Model, Msg(Mdc), defaultModel, subscriptions, update, view)

import Data.User as User exposing (User, decoder)
import Html exposing (Html, text)
import Html.Attributes as Html
import Html.Events as Html
import Json.Decode as Decode exposing (Value)
import Material
import Material.Button as Button
import Material.Options as Options exposing (cs, css, onClick, styled, when)
import Material.Textfield as Textfield
import Material.Textfield.HelperText as Textfield
import Material.Theme as Theme
import Material.Typography as Typography
import Pages.Page as Page exposing (Page)
import Ports
import Request.User exposing (storeSession)
import Util exposing ((=>))


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
            model ! []

        -- ( model
        -- , storeSession
        --     (User model.message)
        -- )
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
    page.body "Todo"
        [ styled Html.div
            [ css "padding" "24px"
            , Theme.secondary
            , Theme.secondaryBg
            , Theme.textSecondaryOnLight
            ]
            [ text model.email
            ]
        , styled Html.section
            [ css "padding" "16px"
            ]
            [ styled Html.div
                []
                [ Textfield.view (lift << Mdc)
                    "my-text-field"
                    model.mdc
                    [ Textfield.label "Text field"
                    , Options.onInput (lift << UpdateMessage)
                    , css "background-color" "rgba(255, 255, 255, 0.1)"
                    ]
                    []
                , Textfield.helperText
                    [ Textfield.persistent
                    ]
                    [ Html.text "Put some shit in here"
                    ]
                ]
            , styled Html.div
                []
                [ Textfield.view (lift << Mdc)
                    "my-password-field"
                    model.mdc
                    [ Textfield.label "Choose password"
                    , Textfield.password
                    , Textfield.pattern ".{8,}"
                    , Textfield.required
                    , css "background-color" "rgba(255, 255, 255, 0.1)"
                    ]
                    []
                , Textfield.helperText
                    [ Textfield.persistent
                    , Textfield.validationMsg
                    ]
                    [ Html.text "Must be at least 8 characters long"
                    ]
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
