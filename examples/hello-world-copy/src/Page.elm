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
import Material.List as Lists
import Material.Options as Options exposing (Property, cs, css, styled, when)
import Material.Theme as Theme
import Material.TopAppBar as TopAppBar
import Route exposing (Route)


type alias Page m =
    { isLoading : Bool
    , navigate : Maybe Route -> m
    , body : String -> List (Html m) -> Html m
    }


type alias Model m =
    { mdc : Material.Model m
    , drawerOpen : Bool
    }


defaultModel : Model m
defaultModel =
    { mdc = Material.defaultModel
    , drawerOpen = True
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


drawer : (Msg m -> m) -> Model m -> Html m
drawer lift model =
    Drawer.view (lift << Mdc)
        "main-drawer"
        model.mdc
        [ Drawer.open |> when model.drawerOpen
        , Drawer.onClose (lift CloseDrawer)
        ]
        [ Drawer.toolbarSpacer
            []
            []
        , Lists.ul
            [ Drawer.content
            ]
            [ Lists.li []
                [ Lists.graphicIcon [] "inbox"
                , text "Inbox"
                ]
            , Lists.li []
                [ Lists.graphicIcon [] "star"
                , text "Star"
                ]
            , Lists.li []
                [ Lists.graphicIcon [] "send"
                , text "Sent Mail"
                ]
            , Lists.li []
                [ Lists.graphicIcon [] "drafts"
                , text "Drafts"
                ]
            ]
        ]



-- toolbar :
--     (Material.Msg m -> m)
--     -> Material.Index
--     -> Material.Model m
--     -> (Maybe Route -> m)
--     -> Route
--     -> String
--     -> String
--     -> Html m
-- toolbar lift idx mdc navigate route title email =


toolbar :
    (Msg m -> m)
    -> Model m
    -> (Maybe Route -> m)
    -> Route
    -> String
    -> String
    -> Html m
toolbar lift model navigate route title email =
    let
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
            [ TopAppBar.fixed ]
            [ TopAppBar.section
                [ TopAppBar.alignStart
                , Theme.background
                , css "color" "rgba(255, 255, 255, 0.5)"
                , css "background-color" "#222222"
                ]
                [ TopAppBar.navigationIcon
                    [ css "color" "rgba(255, 255, 255, 0.5)"
                    , Options.onClick (lift OpenDrawer)
                    ]
                    "menu"
                , TopAppBar.title
                    [ cs "catalog-title"
                    , css "margin-left"
                        (if route == Route.Home then
                            "8px"
                         else
                            "24"
                        )
                    , css "font-family" "'Monoton', 'Roboto Mono', monospace"
                    ]
                    [ text title ]
                ]
            , TopAppBar.section
                [ TopAppBar.alignEnd
                , Theme.background
                , css "color" "rgba(255, 255, 255, 0.5)"
                , css "background-color" "#222222"
                ]
                [ viewSignIn
                , TopAppBar.actionItem
                    [ css "color" "rgba(255, 255, 255, 0.5)"
                    ]
                    "file_download"
                , TopAppBar.actionItem
                    [ css "color" "rgba(255, 255, 255, 0.5)"
                    ]
                    "print"
                , TopAppBar.actionItem
                    [ css "color" "rgba(255, 255, 255, 0.5)"
                    ]
                    "bookmark"
                ]
            ]
        , drawer lift model
        ]
