module Pages.Errored exposing (PageLoadError, pageLoadError, view)

{-| The page that renders when there was an error trying to load another page,
for example a Page Not Found error.

It includes a photo I took of a painting on a building in San Francisco,
of a giant walrus exploding the golden gate bridge with laser beams. Pew pew!

-}

-- import Data.Session exposing (Session)

import Html exposing (Html, div, h1, img, main_, p, text)
import Html.Attributes exposing (alt, class, id, tabindex)
import Route exposing (Route)


-- MODEL --


type PageLoadError
    = PageLoadError Model


type alias Model =
    { route : Route
    , errorMessage : String
    }


pageLoadError : Route -> String -> PageLoadError
pageLoadError route errorMessage =
    PageLoadError
        { route = route
        , errorMessage = errorMessage
        }



-- VIEW --


view : PageLoadError -> Html msg
view (PageLoadError model) =
    main_ [ id "content", class "container", tabindex -1 ]
        [ h1 [] [ text "Error Loading Page" ]
        , div [ class "row" ]
            [ p [] [ text model.errorMessage ] ]
        ]