module Page
    exposing
        ( Page
        , toolbar
        )

import Html exposing (Html, text)
import Material
import Material.Button as Button
import Material.Options as Options exposing (Property, cs, css, styled, when)
import Material.Theme as Theme
import Material.TopAppBar as TopAppBar
import Route exposing (Route)


-- type alias Page m =
--     { toolbar : String -> Html m
--     , isLoading : Bool
--     , navigate : Route -> m
--     , body : String -> List (Html m) -> Html m
--     }


type alias Page m =
    { isLoading : Bool
    , navigate : Maybe Route -> m
    , body : String -> List (Html m) -> Html m
    }


type Msg m
    = OpenDrawer
    | CloseDrawer



-- import Material.Drawer.Temporary as Drawer
-- import Material.List as Lists
-- Drawer.view Mdc "my-drawer" model.mdc []
--     [ Drawer.toolbarSpacer [] []
--     , Lists.ul
--           [ Drawer.content
--           ]
--           [ Lists.li []
--                 [ Lists.graphicIcon [] "inbox"
--                 , text "Inbox"
--                 ]
--           , Lists.li []
--                 [ Lists.graphicIcon [] "star"
--                 , text "Star"
--                 ]
--           , Lists.li []
--                 [ Lists.graphicIcon [] "send"
--                 , text "Sent Mail"
--                 ]
--           , Lists.li []
--                 [ Lists.graphicIcon [] "drafts"
--                 , text "Drafts"
--                 ]
--           ]
--     ]
-- toolbar :
--     (Material.Msg m -> m)
--     -> Material.Index
--     -> Material.Model m
--     -> (Url -> m)
--     -> Url
--     -> String
--     -> Html m
-- toolbar lift idx mdc navigate url title =


toolbar :
    (Material.Msg m -> m)
    -> Material.Index
    -> Material.Model m
    -> (Maybe Route -> m)
    -> Route
    -> String
    -> String
    -> Html m
toolbar lift idx mdc navigate route title email =
    let
        viewSignIn =
            case email of
                "" ->
                    Button.view lift
                        "login-link-button"
                        mdc
                        [ Button.link "#login"
                        , css "margin-top" "8px"
                        ]
                        [ text "Sign in" ]

                _ ->
                    Button.view lift
                        "login-link-button"
                        mdc
                        [ Button.link "#login"
                        , css "margin-top" "8px"
                        ]
                        [ text email ]
    in
    styled Html.div
        []
        [ TopAppBar.view
            lift
            idx
            mdc
            [ TopAppBar.fixed ]
            [ TopAppBar.section
                [ TopAppBar.alignStart
                , Theme.background
                , css "color" "rgba(255, 255, 255, 0.3)"
                ]
                [ TopAppBar.navigationIcon
                    [ css "color" "rgba(255, 255, 255, 0.3)"

                    -- , Options.onClick (lift OpenDrawer)
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
                , css "color" "rgba(255, 255, 255, 0.3)"
                ]
                [ viewSignIn
                , TopAppBar.actionItem
                    [ css "color" "rgba(255, 255, 255, 0.3)"
                    ]
                    "file_download"
                , TopAppBar.actionItem
                    [ css "color" "rgba(255, 255, 255, 0.3)"
                    ]
                    "print"
                , TopAppBar.actionItem
                    [ css "color" "rgba(255, 255, 255, 0.3)"
                    ]
                    "bookmark"
                ]
            ]
        ]



-- fixedAdjust : Material.Index -> Material.Model m -> Options.Property c m
-- fixedAdjust idx mdc =
--     TopAppBar.fixedAdjust
--idx mdc
-- hero : List (Property c m) -> List (Html m) -> Html m
-- hero options =
--     styled Html.section
--         (List.reverse
--             -- TODO: dang it
--             (cs "hero"
--                 :: css "display" "-webkit-box"
--                 :: css "display" "-ms-flexbox"
--                 :: css "display" "flex"
--                 :: css "-webkit-box-orient" "horizontal"
--                 :: css "-webkit-box-direction" "normal"
--                 :: css "-ms-flex-flow" "row nowrap"
--                 :: css "flex-flow" "row nowrap"
--                 :: css "-webkit-box-align" "center"
--                 :: css "-ms-flex-align" "center"
--                 :: css "align-items" "center"
--                 :: css "-webkit-box-pack" "center"
--                 :: css "-ms-flex-pack" "center"
--                 :: css "justify-content" "center"
--                 :: css "height" "360px"
--                 :: css "min-height" "360px"
--                 :: css "background-color" "rgba(0, 0, 0, 0.05)"
--                 :: options
--             )
--         )
