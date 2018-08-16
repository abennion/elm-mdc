module Views.Toolbar
    exposing
        ( Model
        , Msg(CloseDrawer, Mdc, OpenDrawer)
        , Toolbar
        , defaultModel
        , update
        , view
        )

import Data.User as User exposing (User)
import Html exposing (Html, text)
import Material
import Material.Button as Button
import Material.LinearProgress as LinearProgress
import Material.Options as Options exposing (Property, cs, css, styled, when)
import Material.TopAppBar as TopAppBar
import Pages.Page as Page exposing (Page)
import Route exposing (Route)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Views.View exposing (Context)


-- MODEL


type alias Model m =
    { mdc : Material.Model m
    }


defaultModel : Model m
defaultModel =
    { mdc = Material.defaultModel
    }


type Msg m
    = Mdc (Material.Msg m)
    | OpenDrawer
    | CloseDrawer


type Toolbar
    = Other
    | Home
    | Login



-- UPDATE


update : (Msg m -> m) -> Msg m -> Model m -> ( Model m, Cmd m )
update lift msg model =
    case msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model

        OpenDrawer ->
            ( model, Cmd.none )

        CloseDrawer ->
            ( model, Cmd.none )



-- VIEW


view : (Msg m -> m) -> Context m -> Model m -> Html m
view lift context model =
    let
        page =
            context.page

        _ =
            Debug.log "Toolbar.view page" page

        spinner isLoading =
            case isLoading of
                True ->
                    LinearProgress.view
                        [ LinearProgress.buffered 0.0 0.0
                        , LinearProgress.indeterminate
                        , cs "demo-linear-progress--custom"
                        , css "margin-left" "24px"
                        , css "width" "64px"
                        ]
                        []

                False ->
                    Html.text ""

        leftSide =
            case context.page of
                Page.Home ->
                    [ TopAppBar.title
                        [ css "font-family" "'Roboto Mono', Monoton, monospace"
                        , css "text-transform" "uppercase"
                        , css "font-weight" "400"
                        ]
                        [ Html.text context.title
                        ]
                    ]

                _ ->
                    [ TopAppBar.actionItem
                        [ Options.onClick (context.setRoute (Just Route.Home))
                        ]
                        "home"
                    , TopAppBar.title
                        [ css "font-family" "'Roboto Mono', Monoton, monospace"
                        , css "text-transform" "uppercase"
                        , css "font-weight" "400"
                        ]
                        [ Html.text context.title
                        ]
                    ]

        -- viewSignIn : Toolbar -> Maybe User -> List (Html msg)
        -- viewSignIn page user =
        --     let
        --         linkTo =
        --             navbarLink page
        --     in
        --     case user of
        --         Nothing ->
        --             [ linkTo Route.Login [ text "Sign in" ]
        --             , linkTo Route.Register [ text "Sign up" ]
        --             ]
        --         Just user ->
        --             [ linkTo Route.NewArticle [ i [ class "ion-compose" ] [], text " New Post" ]
        --             , linkTo Route.Settings [ i [ class "ion-gear-a" ] [], text " Settings" ]
        --             , linkTo
        --                 (Route.Profile user.username)
        --                 [ img [ class "user-pic", UserPhoto.src user.image ] []
        --                 , User.usernameToHtml user.username
        --                 ]
        --             , linkTo Route.Logout [ text "Sign out" ]
        --             ]
        viewSignIn =
            case context.user of
                Nothing ->
                    Button.view
                        (lift << Mdc)
                        "login-link-button"
                        model.mdc
                        [ Button.link "#login"
                        ]
                        [ Html.text "Sign in" ]

                Just user ->
                    Button.view
                        (lift << Mdc)
                        "login-link-button"
                        model.mdc
                        [ Button.link "#login"
                        , Options.onClick (context.setRoute (Just Route.Login))
                        ]
                        [ Html.text user.email ]
    in
    styled Html.div
        [ cs "demo-top-app-bar--custom"
        ]
        [ TopAppBar.view
            (lift << Mdc)
            "main-topappbar"
            model.mdc
            [ TopAppBar.fixed
            ]
            [ TopAppBar.section
                [ TopAppBar.alignStart
                , cs "demo-top-app-bar--custom"
                ]
                (List.concat
                    [ [ TopAppBar.navigationIcon
                            [ Options.onClick (lift OpenDrawer)
                            ]
                            "menu"
                      ]
                    , leftSide
                    ]
                )
            , case context.isLoading of
                True ->
                    TopAppBar.section
                        [ cs "demo-top-app-bar--custom"
                        , css "display" "flex"
                        , css "align-items" "center"
                        , css "justify-content" "center"
                        ]
                        [ svg
                            [ class "mdc-circular-progress"
                            , viewBox "25 25 50 50"
                            ]
                            [ Svg.circle
                                [ class "mdc-circular-progress__path"
                                , cx "50"
                                , cy "50"
                                , r "20"
                                , fill "none"
                                , strokeWidth "2"
                                , strokeMiterlimit "10"
                                ]
                                []
                            ]
                        ]

                False ->
                    Html.text ""
            , TopAppBar.section
                [ TopAppBar.alignEnd
                , cs "demo-top-app-bar--custom"
                ]
                [ viewSignIn
                , Button.view
                    (lift << Mdc)
                    "sign-out-button"
                    model.mdc
                    [ Options.onClick (context.setUser Nothing)
                    ]
                    [ Html.text "Sign out" ]
                , TopAppBar.actionItem
                    []
                    "more_vert"
                ]
            ]
        ]
