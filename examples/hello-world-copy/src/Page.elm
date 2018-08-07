module Page
    exposing
        ( Model
        , Msg(Mdc)
        , Page
        , defaultModel
        , drawer
        , toolbar
        , update
        )

import Html exposing (Html, text)
import Html.Attributes as Html
import Material
import Material.Button as Button
import Material.Drawer.Temporary as Drawer
import Material.LinearProgress as LinearProgress
import Material.List as Lists
import Material.Menu as Menu
import Material.Options as Options exposing (Property, cs, css, styled, when)
import Material.Theme as Theme
import Material.TopAppBar as TopAppBar
import Route exposing (Route)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Page m =
    { isLoading : Bool
    , navigate : Maybe Route -> m
    , body : String -> Bool -> List (Html m) -> Html m
    }


type alias Model m =
    { mdc : Material.Model m
    , drawerOpen : Bool
    }


defaultModel : Model m
defaultModel =
    { mdc = Material.defaultModel
    , drawerOpen = False
    }


type Msg m
    = Mdc (Material.Msg m)
    | OpenDrawer
    | CloseDrawer


update : (Msg m -> m) -> Msg m -> Model m -> ( Model m, Cmd m )
update lift msg model =
    case msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model

        OpenDrawer ->
            ( { model | drawerOpen = True }
            , Cmd.none
            )

        CloseDrawer ->
            ( { model | drawerOpen = False }
            , Cmd.none
            )



-- TODO: Make drawer and toolbar their own view classes, or something like
-- that. The page module should just handle state, I guess.


drawer :
    (Msg m -> m)
    -> Model m
    -> (Maybe Route -> m)
    -> Html m
drawer lift model setRoute =
    styled Html.div
        [ cs "demo-drawer--custom"
        ]
        [ Drawer.view (lift << Mdc)
            "main-drawer"
            model.mdc
            [ Drawer.open |> when model.drawerOpen
            , Drawer.onClose (lift CloseDrawer)
            ]
            [ Drawer.header
                []
                [ Drawer.headerContent []
                    [ Html.text "Header here"
                    ]
                ]
            , Drawer.toolbarSpacer
                []
                []
            , Lists.ul
                [ Drawer.content
                ]
                [ Lists.li
                    [ Options.onClick (setRoute (Just Route.Home))
                    ]
                    [ Lists.graphicIcon
                        []
                        "home"
                    , Html.text "Home"
                    ]
                , Lists.li
                    [ Options.onClick (setRoute (Just Route.Other))
                    ]
                    [ Lists.graphicIcon
                        [ cs "demo-drawer--custom"
                        ]
                        "link"
                    , Html.text "Other"
                    ]
                , Lists.li
                    []
                    [ Lists.graphicIcon
                        []
                        "send"
                    , Html.text "Sent Mail"
                    ]
                , Lists.li
                    []
                    [ Lists.graphicIcon
                        []
                        "drafts"
                    , Html.text "Drafts"
                    ]
                ]
            ]
        ]


toolbar :
    (Msg m -> m)
    -> Model m
    -> Bool
    -> (Maybe Route -> m)
    -> Route
    -> String
    -> String
    -> Html m
toolbar lift model isLoading navigate route title email =
    let
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

        viewSignIn =
            case email of
                "" ->
                    Button.view
                        (lift << Mdc)
                        "login-link-button"
                        model.mdc
                        [ Button.link "#login"
                        ]
                        [ Html.text "Sign in" ]

                _ ->
                    Button.view
                        (lift << Mdc)
                        "login-link-button"
                        model.mdc
                        [ Button.link "#login"
                        ]
                        [ Html.text email ]
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
                [ TopAppBar.navigationIcon
                    [ Options.onClick (lift OpenDrawer)
                    ]
                    "menu"
                , TopAppBar.title
                    [ css "margin-left"
                        (if route == Route.Home then
                            "8px"
                         else
                            "24"
                        )
                    , css "font-family" "'Roboto Mono', Monoton, monospace"
                    , css "text-transform" "uppercase"
                    , css "font-weight" "400"
                    ]
                    [ Html.text title
                    ]
                ]
            , case isLoading of
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
                , TopAppBar.actionItem
                    []
                    "more_vert"
                ]
            ]
        ]
