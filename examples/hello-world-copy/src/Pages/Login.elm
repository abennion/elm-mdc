module Pages.Login exposing (Model, Msg(..), defaultModel, update, view)

import Data.Session exposing (Session)
import Data.User exposing (User)
import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
import Json.Decode.Pipeline exposing (decode, optional)
import Material
import Material.Button as Button
import Material.Checkbox as Checkbox
import Material.FormField as FormField
import Material.LinearProgress as LinearProgress
import Material.Options as Options exposing (cs, css, styled, when)
import Material.Textfield as Textfield
import Material.Textfield.HelperText as Textfield
import Page exposing (Page)
import Request.User exposing (storeSession)
import Route exposing (Route)
import Views.Form as Form


type Field
    = Form
    | Email
    | Password


{-| Recording validation errors on a per-field basis facilitates displaying
them inline next to the field where the error occurred.

I implemented it this way out of habit, then realized the spec called for
displaying all the errors at the top. I thought about simplifying it, but then
figured it'd be useful to show how I would normally model this data - assuming
the intended UX was to render errors per field.

(The other part of this is having a view function like this:

viewFormErrors : Field -> List Error -> Html msg

...and it filters the list of errors to render only the ones for the given
Field. This way you can call this:

viewFormErrors Email model.errors

...next to the `email` field, and call `viewFormErrors Password model.errors`
next to the `password` field, and so on.

-}
type alias Error =
    ( Field, String )


type alias Model m =
    { mdc : Material.Model m
    , text : String
    , errors : List Error
    , email : String
    , password : String
    }


defaultModel : Model m
defaultModel =
    { mdc = Material.defaultModel
    , text = "Nothing to see here."
    , errors = []
    , email = ""
    , password = ""
    }


type Msg m
    = Mdc (Material.Msg m)
    | Click String
    | SetEmail String
    | SetPassword String
    | SubmitForm
    | LoginCompleted (Result Http.Error User)


update : (Msg m -> m) -> Msg m -> Model m -> ( Model m, Cmd m )
update lift msg model =
    case msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model

        Click text ->
            ( { model | text = text }
            , Cmd.none
            )

        SetEmail email ->
            ( { model | email = email }, Cmd.none )

        SetPassword password ->
            ( { model | password = password }, Cmd.none )

        SubmitForm ->
            ( { model | errors = [] }
            , Http.send (lift << LoginCompleted) (Request.User.login model)
            )

        LoginCompleted (Err error) ->
            let
                errorMessages =
                    case error of
                        Http.BadStatus response ->
                            response.body
                                |> decodeString (field "errors" errorsDecoder)
                                |> Result.withDefault []

                        _ ->
                            [ "unable to perform login" ]
            in
            ( { model | errors = List.map (\errorMessage -> ( Form, errorMessage )) errorMessages }
            , Cmd.none
            )

        LoginCompleted (Ok user) ->
            ( model
            , Cmd.batch
                [ storeSession user
                , Route.modifyUrl Route.Home
                ]
            )


view : (Msg m -> m) -> Page m -> Model m -> Html m
view lift page model =
    page.body "Login"
        page.isLoading
        [ styled Html.div
            [ css "padding" "24px"
            ]
            [ styled Html.h1 [] [ text "Sign in" ]
            , styled Html.p
                []
                [ Html.a
                    [ href "http://google.com" ]
                    [ text "Need an account?" ]
                ]
            , Form.viewErrors model.errors
            , viewForm lift page model
            ]
        ]


viewForm : (Msg m -> m) -> Page m -> Model m -> Html m
viewForm lift page model =
    styled Html.section
        (List.reverse
            -- TODO: dang it
            (cs "hero"
                :: css "display" "-webkit-box"
                :: css "display" "-ms-flexbox"
                :: css "display" "flex"
                :: css "-webkit-box-orient" "horizontal"
                :: css "-webkit-box-direction" "normal"
                :: css "-ms-flex-flow" "row nowrap"
                :: css "flex-flow" "row nowrap"
                :: css "-webkit-box-align" "center"
                :: css "-ms-flex-align" "center"
                :: css "align-items" "center"
                :: css "-webkit-box-pack" "center"
                :: css "-ms-flex-pack" "center"
                :: css "justify-content" "center"
                -- :: css "height" "360px"
                :: css "min-height" "360px"
                :: [ css "background-color" "rgba(0, 0, 0, 0.05)" ]
            )
        )
        [ styled Html.div
            [ css "width" "360px"
            ]
            [ Html.div
                []
                [ Textfield.view (lift << Mdc)
                    "login-email"
                    model.mdc
                    [ Textfield.label "Email"
                    , Textfield.required
                    , Options.onInput (lift << SetEmail)
                    ]
                    []
                , Textfield.helperText
                    [ Textfield.persistent
                    ]
                    [ Html.text "Valid email address"
                    ]
                ]
            , Html.br [] []
            , Html.div
                []
                [ Textfield.view (lift << Mdc)
                    "login-password"
                    model.mdc
                    [ Textfield.label "Choose password"
                    , Textfield.password
                    , Textfield.pattern ".{16,}"
                    , Textfield.required
                    , Options.onInput (lift << SetPassword)
                    ]
                    []
                , Textfield.helperText
                    [ Textfield.persistent
                    , Textfield.validationMsg
                    ]
                    [ Html.text "Must be at least 16 characters long"
                    ]
                ]
            , Html.br [] []
            , Button.view (lift << Mdc)
                "login-submit"
                model.mdc
                [ Button.raised
                , Options.onClick (lift SubmitForm)
                ]
                [ text "Submit"
                ]
            ]
        ]



-- Html.div []
--     [ Textfield.view (lift << Mdc) index model.mdc
--       [ Textfield.label "Choose password"
--       , Textfield.password
--       , Textfield.pattern ".{8,}"
--       , Textfield.required
--       , Textfield.disabled |> when state.disabled
--       , Textfield.dense |> when state.dense
--       ]
--       []
--     , Textfield.helperText
--       [ Textfield.persistent
--       , Textfield.validationMsg
--       ]
--       [ Html.text "Must be at least 8 characters long"
--       ]
--     ]
-- styled Html.form
--     [ Options.onSubmit SubmitForm
--     ]
--     [ styled Form.input
--         [ class "form-control-lg"
--         , placeholder "Email"
--         , Options.onInput SetEmail
--         ]
--         []
--     , styled Form.password
--         [ class "form-control-lg"
--         , placeholder "Password"
--         , Options.onInput SetPassword
--         ]
--         []
--     , button [ class "btn btn-lg btn-primary pull-xs-right" ]
--         [ text "Sign in" ]
--     ]
-- div [ class "auth-page" ]
--     [ div [ class "container page" ]
--         [ div [ class "row" ]
--             [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
--                 [ h1 [ class "text-xs-center" ] [ text "Sign in" ]
--                 , p [ class "text-xs-center" ]
--                     [ a [ Route.href Route.Register ]
--                         [ text "Need an account?" ]
--                     ]
--                 , Form.viewErrors model.errors
--                 -- , viewForm
--                 ]
--             ]
--         ]
--     ]
-- view : (Msg m -> m) -> Page m -> Model m -> Html m
-- view lift page model =
--     let
--         fakeText =
--             "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do"
--                 ++ " eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut"
--                 ++ " enim ad minim veniam, quis nostrud exercitation ullamco laboris"
--                 ++ " nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in"
--                 ++ " reprehenderit in voluptate velit esse cillum dolore eu fugiat"
--                 ++ " nulla pariatur. Excepteur sint occaecat cupidatat non proident,"
--                 ++ " sunt in culpa qui officia deserunt mollit anim id est laborum."
--     in
--     page.body "Login"
--         page.isLoading
--         [ styled Html.div
--             []
--             [ styled Html.h2
--                 []
--                 [ text model.text
--                 ]
--             , styled Html.h2
--                 []
--                 [ text ("Is loading: " ++ toString page.isLoading)
--                 ]
--             , Button.view (lift << Mdc)
--                 "my-button"
--                 model.mdc
--                 [ Button.ripple
--                 , Options.onClick (page.navigate (Just Route.Home))
--                 ]
--                 [ text "Home!"
--                 ]
--             , styled Html.div
--                 [ css "padding" "24px"
--                 ]
--                 [ styled Html.p
--                     [ css "padding" "8px" ]
--                     [ text fakeText ]
--                 ]
--             ]
--         ]
-- modelValidator : Validator Error Model
-- modelValidator =
--     Validate.all
--         [ ifBlank .email (Email => "email can't be blank.")
--         , ifBlank .password (Password => "password can't be blank.")
--         ]


errorsDecoder : Decoder (List String)
errorsDecoder =
    decode (\emailOrPassword email username password -> List.concat [ emailOrPassword, email, username, password ])
        |> optionalError "email or password"
        |> optionalError "email"
        |> optionalError "username"
        |> optionalError "password"


optionalError : String -> Decoder (List String -> a) -> Decoder a
optionalError fieldName =
    let
        errorToString errorMessage =
            String.join " " [ fieldName, errorMessage ]
    in
    optional fieldName (Decode.list (Decode.map errorToString string)) []
