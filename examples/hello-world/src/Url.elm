module Url
    exposing
        ( ToolbarPage(..)
        , TopAppBarPage(..)
        , Url(..)
        , fromString
        , toString
        )


type Url
    = Button
    | Chips
    | Error404 String


type ToolbarPage
    = DefaultToolbar
    | FixedToolbar
    | MenuToolbar
    | WaterfallToolbar
    | DefaultFlexibleToolbar
    | WaterfallFlexibleToolbar
    | WaterfallToolbarFix


type TopAppBarPage
    = StandardTopAppBar
    | FixedTopAppBar
    | DenseTopAppBar
    | ProminentTopAppBar
    | ShortTopAppBar
    | ShortCollapsedTopAppBar


toString : Url -> String
toString url =
    let
        toolbarCase toolbar =
            case toolbar of
                Nothing ->
                    "#toolbar"

                Just DefaultToolbar ->
                    "#toolbar/default-toolbar"

                Just FixedToolbar ->
                    "#toolbar/fixed-toolbar"

                Just MenuToolbar ->
                    "#toolbar/menu-toolbar"

                Just WaterfallToolbar ->
                    "#toolbar/waterfall-toolbar"

                Just DefaultFlexibleToolbar ->
                    "#toolbar/default-flexible-toolbar"

                Just WaterfallFlexibleToolbar ->
                    "#toolbar/waterfall-flexible-toolbar"

                Just WaterfallToolbarFix ->
                    "#toolbar/waterfall-toolbar-fix-last-row"

        topAppBarCase topAppBar =
            case topAppBar of
                Nothing ->
                    "#top-app-bar"

                Just StandardTopAppBar ->
                    "#top-app-bar/standard"

                Just FixedTopAppBar ->
                    "#top-app-bar/fixed"

                Just DenseTopAppBar ->
                    "#top-app-bar/dense"

                Just ProminentTopAppBar ->
                    "#top-app-bar/prominent"

                Just ShortTopAppBar ->
                    "#top-app-bar/short"

                Just ShortCollapsedTopAppBar ->
                    "#top-app-bar/short-collapsed"
    in
    case url of
        Button ->
            "#buttons"

        Chips ->
            "#chips"

        Error404 requestedHash ->
            requestedHash


fromString : String -> Url
fromString str =
    let
        case1 str =
            case String.uncons str of
                Nothing ->
                    Just <| Button

                Just ( '#', "" ) ->
                    Just <| Button

                Just ( '#', "buttons" ) ->
                    Just <| Button

                Just ( '#', "chips" ) ->
                    Just <| Chips

                _ ->
                    Nothing
    in
    [ case1 str
    ]
        |> List.filterMap identity
        |> List.head
        |> Maybe.withDefault (Error404 str)
