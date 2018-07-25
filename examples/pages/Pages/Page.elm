module Pages.Page
    exposing
        ( Page
        , fixedAdjust
        , hero
        , toolbar
        )

import Html exposing (Html, text)
import Html.Attributes as Html
import Material
import Material.Icon as Icon
import Material.Menu as Menu
import Material.Options as Options exposing (Property, cs, css, styled, when)
import Material.Theme as Theme
import Material.Toolbar as Toolbar
import Pages.Url as Url exposing (Url)


type alias Page m =
    { toolbar : String -> Html m
    , fixedAdjust : Options.Property () m
    , navigate : Url -> m
    , body : String -> List (Html m) -> Html m
    }



-- toolbar = Page.toolbar Mdc "page-toolbar" model.mdc Navigate model.url


toolbar :
    (Material.Msg m -> m)
    -> Material.Index
    -> Material.Model m
    -> (Url -> m)
    -> Url
    -> String
    -> Html m
toolbar lift idx mdc navigate url title =
    Toolbar.view lift
        idx
        mdc
        [ Toolbar.fixed
        , cs "catalog-top-app-bar"
        ]
        [ Toolbar.row
            [ Theme.background
            ]
            [ Toolbar.section
                [ Toolbar.alignStart
                ]
                [ Icon.view
                    [ Toolbar.menuIcon
                    , Menu.attach lift "my-menu"
                    , Theme.secondary
                    ]
                    "menu"
                , Menu.view
                    lift
                    "my-menu"
                    mdc
                    []
                    (Menu.ul []
                        [ Menu.li
                            [ Menu.onSelect (navigate Url.ImageList)
                            ]
                            [ text "Images"
                            ]
                        , Menu.li
                            [ Menu.onSelect (navigate Url.Home)
                            ]
                            [ text "Home"
                            ]
                        ]
                    )
                , styled Html.div
                    [ cs "catalog-back"
                    , css "padding-right" "24px"
                    ]
                    [ case url of
                        Url.Home ->
                            styled Html.img
                                [ cs "mdc-toolbar__menu-icon"
                                , Options.attribute (Html.src "images/ic_component_24px_white.svg")
                                ]
                                []

                        _ ->
                            Icon.view
                                [ Options.onClick (navigate Url.Home)
                                , Toolbar.menuIcon
                                ]
                                "arrow_back"
                    ]
                , Toolbar.title
                    [ cs "cataloge-title"
                    , css "margin-left"
                        (if url == Url.Home then
                            "8px"
                         else
                            "24"
                        )
                    , css "font-family" "'Roboto Mono', monospace"
                    ]
                    [ text title ]
                ]
            ]
        ]


fixedAdjust : Material.Index -> Material.Model m -> Options.Property c m
fixedAdjust idx mdc =
    Toolbar.fixedAdjust idx mdc


hero : List (Property c m) -> List (Html m) -> Html m
hero options =
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
                :: css "height" "360px"
                :: css "min-height" "360px"
                :: css "background-color" "rgba(0, 0, 0, 0.05)"
                :: options
            )
        )
