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
import Material
import Material.Button as Button
import Material.Drawer.Temporary as Drawer
import Material.LinearProgress as LinearProgress
import Material.List as Lists
import Material.Options as Options exposing (Property, cs, css, styled, when)
import Material.TopAppBar as TopAppBar
import Route exposing (Route)


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
    let
        backgroundColor =
            cs "mdc-theme--dark-background"

        color =
            css "color" "rgba(255, 255, 255, 0.5)"
    in
    Drawer.view (lift << Mdc)
        "main-drawer"
        model.mdc
        [ Drawer.open |> when model.drawerOpen
        , Drawer.onClose (lift CloseDrawer)
        , backgroundColor
        ]
        [ Drawer.header
            [ backgroundColor
            , color
            ]
            [ Drawer.headerContent []
                [ text "Header here"
                ]
            ]
        , Drawer.toolbarSpacer
            [ backgroundColor
            , color
            ]
            []
        , Lists.ul
            [ Drawer.content
            , backgroundColor
            , color
            ]
            [ Lists.li
                [ Options.onClick (setRoute (Just Route.Home))
                ]
                [ Lists.graphicIcon
                    [ color
                    ]
                    "inbox"
                , text "Home"
                ]
            , Lists.li
                [ Options.onClick (setRoute (Just Route.Other))
                ]
                [ Lists.graphicIcon
                    [ color
                    ]
                    "star"
                , text "Other"
                ]
            , Lists.li
                []
                [ Lists.graphicIcon
                    [ color
                    ]
                    "send"
                , text "Sent Mail"
                ]
            , Lists.li
                []
                [ Lists.graphicIcon
                    [ color
                    ]
                    "drafts"
                , text "Drafts"
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
        backgroundColor =
            cs "mdc-theme--dark-background"

        color =
            css "color" "rgba(255, 255, 255, 0.5)"

        spinner isLoading =
            case isLoading of
                True ->
                    styled Html.div
                        [ cs "mdc-theme--dark-background"
                        , css "width" "100%"
                        ]
                        [ LinearProgress.view
                            [ LinearProgress.buffered 0.0 0.0
                            , LinearProgress.indeterminate
                            , cs "demo-linear-progress--custom"
                            ]
                            []
                        ]

                False ->
                    text ""

        viewSignIn =
            case email of
                "" ->
                    Button.view
                        (lift << Mdc)
                        "login-link-button"
                        model.mdc
                        [ Button.link "#login"
                        ]
                        [ text "Sign in" ]

                _ ->
                    Button.view
                        (lift << Mdc)
                        "login-link-button"
                        model.mdc
                        [ Button.link "#login"
                        ]
                        [ text email ]
    in
    styled Html.div
        []
        [ TopAppBar.view
            (lift << Mdc)
            "main-topappbar"
            model.mdc
            [ TopAppBar.fixed
            ]
            [ TopAppBar.section
                [ TopAppBar.alignStart
                , backgroundColor
                ]
                [ TopAppBar.navigationIcon
                    [ Options.onClick (lift OpenDrawer)
                    , color
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
                    , color
                    ]
                    [ styled Html.table
                        []
                        [ styled Html.tr
                            []
                            [ styled Html.td
                                []
                                [ text title
                                ]
                            , styled Html.td
                                [ css "padding-left" "16px"
                                , css "width" "100%"
                                ]
                                [ spinner isLoading
                                ]
                            ]
                        ]
                    ]
                ]
            , TopAppBar.section
                [ TopAppBar.alignStart
                , backgroundColor
                ]
                [ spinner isLoading
                ]
            , TopAppBar.section
                [ TopAppBar.alignEnd
                , backgroundColor
                ]
                [ viewSignIn
                , TopAppBar.actionItem
                    [ color
                    ]
                    "file_download"
                , TopAppBar.actionItem
                    [ color
                    ]
                    "print"
                , TopAppBar.actionItem
                    [ color
                    ]
                    "bookmark"
                ]
            ]
        ]
