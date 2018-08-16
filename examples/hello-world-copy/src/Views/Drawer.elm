module Views.Drawer exposing (Model, Msg(..), defaultModel, update, view)

import Html exposing (Html, text)
import Material
import Material.Drawer.Temporary as Drawer
import Material.List as Lists
import Material.Options as Options exposing (Property, cs, css, styled, when)
import Route exposing (Route)
import Views.View exposing (View)


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
    case Debug.log "Drawer.update" msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model

        OpenDrawer ->
            ( { model | drawerOpen = True }, Cmd.none )

        CloseDrawer ->
            ( { model | drawerOpen = False }, Cmd.none )


view : (Msg m -> m) -> View m -> Model m -> Html m
view lift view_ model =
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
                    [ Options.onClick (view_.setRoute (Just Route.Home))
                    ]
                    [ Lists.graphicIcon
                        []
                        "home"
                    , Html.text "Home"
                    ]
                , Lists.li
                    [ Options.onClick (view_.setRoute (Just Route.Other))
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
