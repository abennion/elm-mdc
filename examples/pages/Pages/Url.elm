module Pages.Url
    exposing
        ( Url(..)
        , fromString
        , toString
        )


type Url
    = Home
    | ImageList
    | Select
    | Theme
    | Error404 String


toString : Url -> String
toString url =
    case url of
        Home ->
            "#"

        ImageList ->
            "#images"

        Select ->
            "#select"

        Theme ->
            "#theme"

        Error404 requestedHash ->
            requestedHash


fromString : String -> Url
fromString str =
    let
        case1 str =
            case String.uncons str of
                Nothing ->
                    Just <| Home

                Just ( '#', "" ) ->
                    Just <| Home

                Just ( '#', "images" ) ->
                    Just <| ImageList

                Just ( '#', "select" ) ->
                    Just <| Select

                Just ( '#', "theme" ) ->
                    Just <| Theme

                _ ->
                    Nothing
    in
    [ case1 str
    ]
        |> List.filterMap identity
        |> List.head
        |> Maybe.withDefault (Error404 str)
