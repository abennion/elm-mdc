module Pages.Url
    exposing
        ( Url(..)
        , fromString
        , toString
        )


type Url
    = Home
    | ImageList
    | Login
    | Select
    | Theme
    | Todo
    | Error404 String


toString : Url -> String
toString url =
    case url of
        Home ->
            "#"

        ImageList ->
            "#images"

        Login ->
            "#login"

        Select ->
            "#select"

        Theme ->
            "#theme"

        Todo ->
            "#todo"

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

                Just ( '#', "login" ) ->
                    Just <| Login

                Just ( '#', "select" ) ->
                    Just <| Select

                Just ( '#', "theme" ) ->
                    Just <| Theme

                Just ( '#', "todo" ) ->
                    Just <| Todo

                _ ->
                    Nothing
    in
    [ case1 str
    ]
        |> List.filterMap identity
        |> List.head
        |> Maybe.withDefault (Error404 str)
