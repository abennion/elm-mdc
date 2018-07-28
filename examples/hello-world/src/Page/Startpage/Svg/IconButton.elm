module Page.Startpage.Svg.IconButton exposing (view)

import Html exposing (Html)
import Svg exposing (defs, desc, g, mask, path, polygon, rect, svg, text, title, use)
import Svg.Attributes exposing (d, fill, fillRule, height, id, opacity, points, rx, stroke, strokeWidth, transform, version, viewBox, width, x, xlinkHref, y)


view : Html msg
view =
    svg [ viewBox "0 0 180 180", version "1.1" ]
        [ title []
            [ text "icon_toggle_180px" ]
        , desc []
            [ text "Created with Sketch." ]
        , defs []
            [ rect [ id "__3Wl9gmG__path-1", x "0", y "0", width "180", height "180" ]
                []
            , path [ d "M17,13 L12,13 L12,18 L17,18 L17,13 Z M16,2 L16,4 L8,4 L8,2 L6,2 L6,4 L5,4 C3.89,4 3.01,4.9 3.01,6 L3,20 C3,21.1 3.89,22 5,22 L19,22 C20.1,22 21,21.1 21,20 L21,6 C21,4.9 20.1,4 19,4 L18,4 L18,2 L16,2 Z M19,20 L5,20 L5,9 L19,9 L19,20 Z", id "__3Wl9gmG__path-3" ]
                []
            , path [ d "M11.99,2 C6.47,2 2,6.48 2,12 C2,17.52 6.47,22 11.99,22 C17.52,22 22,17.52 22,12 C22,6.48 17.52,2 11.99,2 Z M12,20 C7.58,20 4,16.42 4,12 C4,7.58 7.58,4 12,4 C16.42,4 20,7.58 20,12 C20,16.42 16.42,20 12,20 Z", id "__3Wl9gmG__path-5" ]
                []
            , polygon [ id "__3Wl9gmG__path-7", points "12.5 7 11 7 11 13 16.25 16.15 17 14.92 12.5 12.25" ]
                []
            , path [ d "M12,5.9 C13.16,5.9 14.1,6.84 14.1,8 C14.1,9.16 13.16,10.1 12,10.1 C10.84,10.1 9.9,9.16 9.9,8 C9.9,6.84 10.84,5.9 12,5.9 Z M12,14.9 C14.97,14.9 18.1,16.36 18.1,17 L18.1,18.1 L5.9,18.1 L5.9,17 C5.9,16.36 9.03,14.9 12,14.9 Z M12,4 C9.79,4 8,5.79 8,8 C8,10.21 9.79,12 12,12 C14.21,12 16,10.21 16,8 C16,5.79 14.21,4 12,4 Z M12,13 C9.33,13 4,14.34 4,17 L4,20 L20,20 L20,17 C20,14.34 14.67,13 12,13 Z", id "__3Wl9gmG__path-9" ]
                []
            , polygon [ id "__3Wl9gmG__path-11", points "8.25166667 17.2516667 4.08166667 13.0816667 2.66666667 14.4966667 8.25166667 20.0816667 20.2516667 8.08166667 18.8366667 6.66666667" ]
                []
            , path [ d "M12,2 C8.13,2 5,5.13 5,9 C5,14.25 12,22 12,22 C12,22 19,14.25 19,9 C19,5.13 15.87,2 12,2 Z M12,11.5 C10.62,11.5 9.5,10.38 9.5,9 C9.5,7.62 10.62,6.5 12,6.5 C13.38,6.5 14.5,7.62 14.5,9 C14.5,10.38 13.38,11.5 12,11.5 Z", id "__3Wl9gmG__path-13" ]
                []
            , path [ d "M12,8 C13.1,8 14,7.1 14,6 C14,4.9 13.1,4 12,4 C10.9,4 10,4.9 10,6 C10,7.1 10.9,8 12,8 L12,8 Z M12,10 C10.9,10 10,10.9 10,12 C10,13.1 10.9,14 12,14 C13.1,14 14,13.1 14,12 C14,10.9 13.1,10 12,10 L12,10 Z M12,16 C10.9,16 10,16.9 10,18 C10,19.1 10.9,20 12,20 C13.1,20 14,19.1 14,18 C14,16.9 13.1,16 12,16 L12,16 Z", id "__3Wl9gmG__path-15" ]
                []
            , polygon [ id "__3Wl9gmG__path-17", points "11 5 11 11 5 11 5 13 11 13 11 19 13 19 13 13 19 13 19 11 13 11 13 5" ]
                []
            , path [ d "M3,18 L21,18 L21,16 L3,16 L3,18 Z M3,13 L21,13 L21,11 L3,11 L3,13 Z M3,6 L3,8 L21,8 L21,6 L3,6 Z", id "__3Wl9gmG__path-19" ]
                []
            , rect [ id "__3Wl9gmG__path-21", x "0", y "0", width "48", height "48", rx "24" ]
                []
            , path [ d "M141.502,39.491 L140.708,39.491 L140.432,39.765 C141.407,40.902 142,42.376 142,43.991 C142,47.581 139.09,50.491 135.5,50.491 C131.91,50.491 129,47.581 129,43.991 C129,40.401 131.91,37.491 135.5,37.491 C137.115,37.491 138.588,38.083 139.725,39.057 L140.001,38.783 L140.001,37.991 L144.999,33 L146.49,34.491 L141.502,39.491 L141.502,39.491 Z M135.5,39.491 C133.014,39.491 131,41.505 131,43.991 C131,46.476 133.014,48.491 135.5,48.491 C137.985,48.491 140,46.476 140,43.991 C140,41.505 137.985,39.491 135.5,39.491 L135.5,39.491 Z", id "__3Wl9gmG__path-23" ]
                []
            ]
        , g [ id "__3Wl9gmG__icon_toggle_180px", stroke "none", strokeWidth "1", fill "none", fillRule "evenodd" ]
            [ mask [ id "__3Wl9gmG__mask-2", fill "white" ]
                [ use [ xlinkHref "#__3Wl9gmG__path-1" ]
                    []
                ]
            , use [ id "__3Wl9gmG__Mask", fill "#FAFAFA", xlinkHref "#__3Wl9gmG__path-1" ]
                []
            , g [ id "__3Wl9gmG__Icon-/-Event-/-Fill", Svg.Attributes.mask "url(#__3Wl9gmG__mask-2)" ]
                [ g [ transform "translate(30.000000, 126.000000)" ]
                    [ mask [ id "__3Wl9gmG__mask-4", fill "white" ]
                        [ use [ xlinkHref "#__3Wl9gmG__path-3" ]
                            []
                        ]
                    , g [ id "__3Wl9gmG__Mask", stroke "none", fill "none", fillRule "nonzero" ]
                        []
                    , g [ id "__3Wl9gmG__?-/-Color-/-Icons-/-Black-/-Inactive", stroke "none", fill "none", Svg.Attributes.mask "url(#__3Wl9gmG__mask-4)", fillRule "evenodd", opacity "0.54" ]
                        [ rect [ id "__3Wl9gmG__Rectangle", fill "#000000", x "0", y "0", width "24", height "24" ]
                            []
                        ]
                    ]
                ]
            , g [ id "__3Wl9gmG__Icon-/-Access-Time-/-Fill", Svg.Attributes.mask "url(#__3Wl9gmG__mask-2)" ]
                [ g [ transform "translate(78.000000, 126.000000)" ]
                    [ mask [ id "__3Wl9gmG__mask-6", fill "white" ]
                        [ use [ xlinkHref "#__3Wl9gmG__path-5" ]
                            []
                        ]
                    , g [ id "__3Wl9gmG__Mask", stroke "none", fill "none", fillRule "nonzero" ]
                        []
                    , g [ id "__3Wl9gmG__?-/-Color-/-Icons-/-Black-/-Inactive", stroke "none", fill "none", Svg.Attributes.mask "url(#__3Wl9gmG__mask-6)", fillRule "evenodd", opacity "0.54" ]
                        [ rect [ id "__3Wl9gmG__Rectangle", fill "#000000", x "0", y "0", width "24", height "24" ]
                            []
                        ]
                    , mask [ id "__3Wl9gmG__mask-8", fill "white" ]
                        [ use [ xlinkHref "#__3Wl9gmG__path-7" ]
                            []
                        ]
                    , g [ id "__3Wl9gmG__Shape", stroke "none", fill "none", fillRule "nonzero" ]
                        []
                    , g [ id "__3Wl9gmG__?-/-Color-/-Icons-/-Black-/-Inactive", stroke "none", fill "none", Svg.Attributes.mask "url(#__3Wl9gmG__mask-8)", fillRule "evenodd", opacity "0.54" ]
                        [ rect [ id "__3Wl9gmG__Rectangle", fill "#000000", x "0", y "0", width "24", height "24" ]
                            []
                        ]
                    ]
                ]
            , g [ id "__3Wl9gmG__Icon-/-Perm-Identity-/-Fill", Svg.Attributes.mask "url(#__3Wl9gmG__mask-2)" ]
                [ g [ transform "translate(78.000000, 79.000000)" ]
                    [ mask [ id "__3Wl9gmG__mask-10", fill "white" ]
                        [ use [ xlinkHref "#__3Wl9gmG__path-9" ]
                            []
                        ]
                    , g [ id "__3Wl9gmG__Shape", stroke "none", fill "none", fillRule "nonzero" ]
                        []
                    , g [ id "__3Wl9gmG__?-/-Color-/-Icons-/-Black-/-87%", stroke "none", fill "none", Svg.Attributes.mask "url(#__3Wl9gmG__mask-10)", fillRule "evenodd", opacity "0.87" ]
                        [ rect [ id "__3Wl9gmG__Rectangle-13", fill "#000000", x "0", y "0", width "24", height "24" ]
                            []
                        ]
                    ]
                ]
            , g [ id "__3Wl9gmG__Icon-/-Done-/-Filled", Svg.Attributes.mask "url(#__3Wl9gmG__mask-2)" ]
                [ g [ transform "translate(78.000000, 30.000000)" ]
                    [ mask [ id "__3Wl9gmG__mask-12", fill "white" ]
                        [ use [ xlinkHref "#__3Wl9gmG__path-11" ]
                            []
                        ]
                    , g [ id "__3Wl9gmG__Mask", stroke "none", fill "none", fillRule "nonzero" ]
                        []
                    , g [ id "__3Wl9gmG__?-/-Color-/-Icons-/-Black-/-Inactive", stroke "none", fill "none", Svg.Attributes.mask "url(#__3Wl9gmG__mask-12)", fillRule "evenodd", opacity "0.54" ]
                        [ rect [ id "__3Wl9gmG__Rectangle", fill "#000000", x "0", y "0", width "24", height "24" ]
                            []
                        ]
                    ]
                ]
            , g [ id "__3Wl9gmG__Icon-/-Location-On-/-Fill", Svg.Attributes.mask "url(#__3Wl9gmG__mask-2)" ]
                [ g [ transform "translate(126.000000, 126.000000)" ]
                    [ mask [ id "__3Wl9gmG__mask-14", fill "white" ]
                        [ use [ xlinkHref "#__3Wl9gmG__path-13" ]
                            []
                        ]
                    , g [ id "__3Wl9gmG__Shape", stroke "none", fill "none", fillRule "nonzero" ]
                        []
                    , g [ id "__3Wl9gmG__?-/-Color-/-Icons-/-Black-/-Inactive", stroke "none", fill "none", Svg.Attributes.mask "url(#__3Wl9gmG__mask-14)", fillRule "evenodd", opacity "0.54" ]
                        [ rect [ id "__3Wl9gmG__Rectangle", fill "#000000", x "0", y "0", width "24", height "24" ]
                            []
                        ]
                    ]
                ]
            , g [ id "__3Wl9gmG__Icon-/-More-Vert-/-Fill", Svg.Attributes.mask "url(#__3Wl9gmG__mask-2)" ]
                [ g [ transform "translate(126.000000, 79.000000)" ]
                    [ mask [ id "__3Wl9gmG__mask-16", fill "white" ]
                        [ use [ xlinkHref "#__3Wl9gmG__path-15" ]
                            []
                        ]
                    , g [ id "__3Wl9gmG__Mask", stroke "none", fill "none" ]
                        []
                    , g [ id "__3Wl9gmG__?-/-Color-/-Icons-/-Black-/-Inactive", stroke "none", fill "none", Svg.Attributes.mask "url(#__3Wl9gmG__mask-16)", fillRule "evenodd", opacity "0.54" ]
                        [ rect [ id "__3Wl9gmG__Rectangle", fill "#000000", x "0", y "0", width "24", height "24" ]
                            []
                        ]
                    ]
                ]
            , g [ id "__3Wl9gmG__Icon-/-Add-/-Fill", Svg.Attributes.mask "url(#__3Wl9gmG__mask-2)" ]
                [ g [ transform "translate(30.000000, 30.000000)" ]
                    [ mask [ id "__3Wl9gmG__mask-18", fill "white" ]
                        [ use [ xlinkHref "#__3Wl9gmG__path-17" ]
                            []
                        ]
                    , g [ id "__3Wl9gmG__Mask", stroke "none", fill "none" ]
                        []
                    , g [ id "__3Wl9gmG__?-/-Color-/-Icons-/-Black-/-Inactive", stroke "none", fill "none", Svg.Attributes.mask "url(#__3Wl9gmG__mask-18)", fillRule "evenodd", opacity "0.54" ]
                        [ rect [ id "__3Wl9gmG__Rectangle", fill "#000000", x "0", y "0", width "24", height "24" ]
                            []
                        ]
                    ]
                ]
            , g [ id "__3Wl9gmG__Icon-/-Menu-/-Fill", Svg.Attributes.mask "url(#__3Wl9gmG__mask-2)" ]
                [ g [ transform "translate(30.000000, 79.000000)" ]
                    [ mask [ id "__3Wl9gmG__mask-20", fill "white" ]
                        [ use [ xlinkHref "#__3Wl9gmG__path-19" ]
                            []
                        ]
                    , g [ id "__3Wl9gmG__Mask", stroke "none", fill "none", fillRule "nonzero" ]
                        []
                    , g [ id "__3Wl9gmG__?-/-Color-/-Icons-/-Black-/-Inactive", stroke "none", fill "none", Svg.Attributes.mask "url(#__3Wl9gmG__mask-20)", fillRule "evenodd", opacity "0.54" ]
                        [ rect [ id "__3Wl9gmG__Rectangle", fill "#000000", x "0", y "0", width "24", height "24" ]
                            []
                        ]
                    ]
                ]
            , g [ id "__3Wl9gmG__Group", Svg.Attributes.mask "url(#__3Wl9gmG__mask-2)" ]
                [ g [ transform "translate(66.000000, 67.000000)" ]
                    [ mask [ id "__3Wl9gmG__mask-22", fill "white" ]
                        [ use [ xlinkHref "#__3Wl9gmG__path-21" ]
                            []
                        ]
                    , g [ id "__3Wl9gmG__Mask", stroke "none", fill "none", opacity "0.16" ]
                        []
                    , g [ id "__3Wl9gmG__?-/-Color-/-Primary-/-500", stroke "none", fill "none", opacity "0.16", Svg.Attributes.mask "url(#__3Wl9gmG__mask-22)", fillRule "evenodd" ]
                        [ g [ transform "translate(-11.000000, -14.000000)", fill "#212121", id "__3Wl9gmG__Rectangle-13" ]
                            [ rect [ x "0", y "0", width "76", height "76" ]
                                []
                            ]
                        ]
                    ]
                ]
            , mask [ id "__3Wl9gmG__mask-24", fill "white" ]
                [ use [ xlinkHref "#__3Wl9gmG__path-23" ]
                    []
                ]
            , use [ id "__3Wl9gmG__Mask", fill "#727272", transform "translate(137.745000, 41.745500) scale(1, -1) translate(-137.745000, -41.745500) ", xlinkHref "#__3Wl9gmG__path-23" ]
                []
            ]
        ]
