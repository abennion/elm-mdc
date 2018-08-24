module Views.Article exposing (view, viewTimestamp, formattedTimestamp)

{-| Viewing a preview of an individual article, excluding its body.
-}

-- import Html.Attributes as Html
-- import Html exposing (Html, text)

import Data.Article exposing (Article)
import Data.UserPhoto as UserPhoto exposing (UserPhoto)
import Date.Format
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src, type_)
import Material
import Material.Button as Button
import Material.Icon as Icon
import Material.LinearProgress as LinearProgress
import Material.List as Lists
import Material.Options as Options exposing (cs, css, styled, when)
import Material.Tabs as TabBar
import Material.Theme as Theme
import Route exposing (Route)
import Views.Article.Favorite as Favorite
import Views.Author


-- VIEWS --


{-| Some pages want to view just the timestamp, not the whole article.
-}
viewTimestamp : Article a -> Html m
viewTimestamp article =
    span [ class "date" ] [ text (formattedTimestamp article) ]



-- mdc idx ...


view : (Article a -> m) -> Article a -> Html m
view toggleFavorite article =
    let
        author =
            article.author
    in
    div
        [ class "article-preview" ]
        [ div [ class "article-meta" ]
            [ a [ Route.href (Route.Profile author.username) ]
                [ img [ UserPhoto.src author.image ] [] ]
            , div [ class "info" ]
                [ Views.Author.view author.username
                , viewTimestamp article
                ]
            , Favorite.button
                toggleFavorite
                article
                [ class "pull-xs-right" ]
                [ text (" " ++ toString article.favoritesCount) ]
            ]
        , a [ class "preview-link", Route.href (Route.Article article.slug) ]
            [ h1 [] [ text article.title ]
            , p [] [ text article.description ]
            , span [] [ text "Read more..." ]
            ]
        ]


avatarWithTextAndIconExample : Html m
avatarWithTextAndIconExample =
    let
        item src primary secondary icon =
            let
                url =
                    "images/" ++ src
            in
            Lists.li []
                [ Lists.graphicImage [] url
                , Lists.text []
                    [ Html.text primary
                    , Lists.secondaryText []
                        [ Html.text secondary
                        ]
                    ]
                , Lists.metaIcon
                    [ css "text-decoration" "none"
                    , css "color" "#ff4081"
                    ]
                    icon
                ]
    in
    Lists.ul
        [ Lists.avatarList
        ]
        [ item "animal3.svg" "Brown Bear" "Jan 9, 2014" "favorite"
        , item "animal1.svg" "Panda" "Jan 9, 2014" "favorite_border"
        , item "animal2.svg" "Sleuth" "Jan 9, 2014" "favorite_border"
        ]



-- twoLineAvatarPlusTextPlusIconExample : Html m
-- twoLineAvatarPlusTextPlusIconExample =
--     let
--         item primary secondary =
--             Lists.li []
--                 [ Lists.graphic []
--                     [ Icon.view [] "favorite"
--                     ]
--                 , Lists.text []
--                     [ Html.text primary
--                     , Lists.secondaryText []
--                         [ Html.text secondary
--                         ]
--                     ]
--                 , Lists.metaIcon [] "info"
--                 ]
--     in
--     styled Html.div
--         [ css "min-width" "340px"
--         , css "max-width" "600px"
--         ]
--         [ Html.node "style"
--             [ type_ "text/css"
--             ]
--             [ Html.text """
-- #two-line-avatar-text-icon-demo .mdc-list-item__start-detail {
--   display: inline-flex;
--   justify-content: center;
--   align-items: center;
--   color: white;
--   background-color: rgba(0,0,0,.26);
-- }
-- #two-line-avatar-text-icon-demo .mdc-list-item__end-detail {
--   color: rgba(0,0,0,.26);
-- }
-- .mdc-theme--dark #two-line-avatar-text-icon-demo .mdc-list-item__start-detail {
--   color: #303030;
--   background-color: rgba(255,255,255,.7);
-- }
-- .mdc-theme--dark #two-line-avatar-text-icon-demo .mdc-list-item__end-detail {
--   color: rgba(255,255,255,.7);
-- }
-- """
--             ]
--         , Lists.ul
--             [ Lists.twoLine
--             , Lists.avatarList
--             , Options.attribute (id "two-line-avatar-text-icon-demo")
--             ]
--             [ item "Photos" "Jan 9, 2014"
--             , item "Recipes" "Jan 17, 2014"
--             , item "Work" "Jan 28, 2014"
--             ]
--         ]
-- INTERNAL --


formattedTimestamp : Article a -> String
formattedTimestamp article =
    Date.Format.format "%B %e, %Y" article.createdAt
