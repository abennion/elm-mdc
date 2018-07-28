module Pages.Home exposing (..)

import Html exposing (Html, div, text)
import Material
import Material.Button as Button
import Material.Options as Options
    exposing
        ( cs
        , css
        , styled
        , when
        )


type alias Model m =
    { mdc : Material.Model m
    , text : String
    }


defaultModel : Model m
defaultModel =
    { mdc = Material.defaultModel
    , text = "Nothing to see here."
    }


type Msg m
    = Mdc (Material.Msg m)
    | Click String


update : (Msg m -> m) -> Msg m -> Model m -> ( Model m, Cmd m )
update lift msg model =
    case msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model

        Click text ->
            ( { model | text = text }
            , Cmd.none
            )


view : (Msg m -> m) -> Model m -> Html m
view lift model =
    styled Html.div
        []
        [ Button.view Mdc
            "my-button"
            model.mdc
            [ Button.ripple
            , Options.onClick (Click "Do the click")
            ]
            [ text "Click me!" ]
        ]
