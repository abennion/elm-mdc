module Page.Startpage.Svg.Chips exposing (view)

import Html exposing (Html)
import Svg exposing (defs, desc, g, mask, path, rect, svg, text, text_, title, tspan, use)
import Svg.Attributes exposing (d, fill, fillOpacity, fillRule, fontFamily, fontSize, fontWeight, height, id, letterSpacing, opacity, rx, stroke, strokeWidth, transform, version, viewBox, width, x, xlinkHref, y)


view : Html msg
view =
    svg [ viewBox "0 0 180 180", version "1.1" ]
        [ title []
            [ text "chips_180px" ]
        , desc []
            [ text "Created with Sketch." ]
        , defs []
            [ rect [ id "__2vBNHao__path-1", x "0", y "0", width "180", height "180" ]
                []
            , rect [ id "__2vBNHao__path-3", x "0", y "0", width "138", height "32", rx "16" ]
                []
            , path [ d "M13,11.87 L11.87,13 L9,10.13 L6.13,13 L5,11.87 L7.87,9 L5,6.13 L6.13,5 L9,7.87 L11.87,5 L13,6.13 L10.13,9 L13,11.87 Z M9,1 C4.58,1 1,4.58 1,9 C1,13.42 4.58,17 9,17 C13.42,17 17,13.42 17,9 C17,4.58 13.42,1 9,1 L9,1 Z", id "__2vBNHao__path-5" ]
                []
            , rect [ id "__2vBNHao__path-7", x "0", y "0", width "145", height "32", rx "16" ]
                []
            , rect [ id "__2vBNHao__path-9", x "0", y "0", width "145", height "32", rx "16" ]
                []
            , rect [ id "__2vBNHao__path-11", x "0", y "0", width "145", height "32", rx "16" ]
                []
            , rect [ id "__2vBNHao__path-13", x "0", y "0", width "145", height "32", rx "16" ]
                []
            , rect [ id "__2vBNHao__path-15", x "0", y "0", width "145", height "32", rx "16" ]
                []
            , rect [ id "__2vBNHao__path-17", x "0", y "0", width "145", height "32", rx "16" ]
                []
            , rect [ id "__2vBNHao__path-19", x "0", y "0", width "145", height "32", rx "16" ]
                []
            , rect [ id "__2vBNHao__path-21", x "0", y "0", width "145", height "32", rx "16" ]
                []
            , rect [ id "__2vBNHao__path-23", x "0", y "0", width "145", height "32", rx "16" ]
                []
            ]
        , g [ id "__2vBNHao__chips_180px", stroke "none", strokeWidth "1", fill "none", fillRule "evenodd" ]
            [ g [ id "__2vBNHao__Group" ]
                [ mask [ id "__2vBNHao__mask-2", fill "white" ]
                    [ use [ xlinkHref "#__2vBNHao__path-1" ]
                        []
                    ]
                , use [ id "__2vBNHao__Mask", fill "#FAFAFA", xlinkHref "#__2vBNHao__path-1" ]
                    []
                , g [ id "__2vBNHao__Group-6", Svg.Attributes.mask "url(#__2vBNHao__mask-2)" ]
                    [ g [ transform "translate(-215.000000, -14.000000)" ]
                        [ g [ id "__2vBNHao__Chips-/-Input-/-Text-only-/-Enabled", strokeWidth "1", fillRule "evenodd", transform "translate(237.000000, 88.000000)" ]
                            [ mask [ id "__2vBNHao__mask-4", fill "white" ]
                                [ use [ xlinkHref "#__2vBNHao__path-3" ]
                                    []
                                ]
                            , use [ id "__2vBNHao__Surface", fill "#FAFAFA", xlinkHref "#__2vBNHao__path-3" ]
                                []
                            , g [ id "__2vBNHao__?-/-Color-/-Icons-/-Black-/-Active", Svg.Attributes.mask "url(#__2vBNHao__mask-4)", fill "#000000", opacity "0.87" ]
                                [ rect [ id "__2vBNHao__Rectangle-13", x "0", y "0", width "138", height "32" ]
                                    []
                                ]
                            , text_ [ id "__2vBNHao__Body-2", Svg.Attributes.mask "url(#__2vBNHao__mask-4)", fontFamily "Roboto-Regular, Roboto", fontSize "14", fontWeight "normal", letterSpacing "0.25", fill "#FFFFFF", fillOpacity "0.87" ]
                                [ tspan [ x "12", y "19" ]
                                    [ text "Enabled Entity" ]
                                ]
                            , g [ id "__2vBNHao__Icon-/-Cancel-/-Fill", Svg.Attributes.mask "url(#__2vBNHao__mask-4)" ]
                                [ g [ transform "translate(112.000000, 7.000000)" ]
                                    [ mask [ id "__2vBNHao__mask-6", fill "white" ]
                                        [ use [ xlinkHref "#__2vBNHao__path-5" ]
                                            []
                                        ]
                                    , g [ id "__2vBNHao__Mask", stroke "none", fill "none" ]
                                        []
                                    , g [ id "__2vBNHao__?-/-Color-/-Icons-/-White-/-100%", stroke "none", fill "none", Svg.Attributes.mask "url(#__2vBNHao__mask-6)", fillRule "evenodd" ]
                                        [ rect [ id "__2vBNHao__Rectangle-13", fill "#FFFFFF", x "0", y "0", width "18", height "18" ]
                                            []
                                        ]
                                    ]
                                ]
                            ]
                        , g [ id "__2vBNHao__Chips-/-Action-/-with-Image-/-Enabled", transform "translate(157.000000, 132.000000)" ]
                            [ mask [ id "__2vBNHao__mask-8", fill "white" ]
                                [ use [ xlinkHref "#__2vBNHao__path-7" ]
                                    []
                                ]
                            , g [ id "__2vBNHao__Surface" ]
                                []
                            , g [ id "__2vBNHao__?-/-Color-/-Icons-/-Black-/-Disabled", opacity "0.119999997", Svg.Attributes.mask "url(#__2vBNHao__mask-8)", fill "#000000", fillRule "evenodd" ]
                                [ rect [ id "__2vBNHao__Rectangle", opacity "0.38", x "0", y "0", width "145", height "32" ]
                                    []
                                ]
                            ]
                        , g [ id "__2vBNHao__Chips-/-Action-/-with-Image-/-Enabled", transform "translate(237.000000, 0.000000)" ]
                            [ mask [ id "__2vBNHao__mask-10", fill "white" ]
                                [ use [ xlinkHref "#__2vBNHao__path-9" ]
                                    []
                                ]
                            , g [ id "__2vBNHao__Surface" ]
                                []
                            , g [ id "__2vBNHao__?-/-Color-/-Icons-/-Black-/-Disabled", opacity "0.119999997", Svg.Attributes.mask "url(#__2vBNHao__mask-10)", fill "#000000", fillRule "evenodd" ]
                                [ rect [ id "__2vBNHao__Rectangle", opacity "0.38", x "0", y "0", width "145", height "32" ]
                                    []
                                ]
                            ]
                        , g [ id "__2vBNHao__Chips-/-Action-/-with-Image-/-Enabled", transform "translate(237.000000, 176.000000)" ]
                            [ mask [ id "__2vBNHao__mask-12", fill "white" ]
                                [ use [ xlinkHref "#__2vBNHao__path-11" ]
                                    []
                                ]
                            , g [ id "__2vBNHao__Surface" ]
                                []
                            , g [ id "__2vBNHao__?-/-Color-/-Icons-/-Black-/-Disabled", opacity "0.119999997", Svg.Attributes.mask "url(#__2vBNHao__mask-12)", fill "#000000", fillRule "evenodd" ]
                                [ rect [ id "__2vBNHao__Rectangle", opacity "0.38", x "0", y "0", width "145", height "32" ]
                                    []
                                ]
                            ]
                        , g [ id "__2vBNHao__Chips-/-Action-/-with-Image-/-Enabled", transform "translate(80.000000, 176.000000)" ]
                            [ mask [ id "__2vBNHao__mask-14", fill "white" ]
                                [ use [ xlinkHref "#__2vBNHao__path-13" ]
                                    []
                                ]
                            , g [ id "__2vBNHao__Surface" ]
                                []
                            , g [ id "__2vBNHao__?-/-Color-/-Icons-/-Black-/-Disabled", opacity "0.119999997", Svg.Attributes.mask "url(#__2vBNHao__mask-14)", fill "#000000", fillRule "evenodd" ]
                                [ rect [ id "__2vBNHao__Rectangle", opacity "0.38", x "0", y "0", width "145", height "32" ]
                                    []
                                ]
                            ]
                        , g [ id "__2vBNHao__Chips-/-Action-/-with-Image-/-Enabled", transform "translate(80.000000, 88.000000)" ]
                            [ mask [ id "__2vBNHao__mask-16", fill "white" ]
                                [ use [ xlinkHref "#__2vBNHao__path-15" ]
                                    []
                                ]
                            , g [ id "__2vBNHao__Surface" ]
                                []
                            , g [ id "__2vBNHao__?-/-Color-/-Icons-/-Black-/-Disabled", opacity "0.119999997", Svg.Attributes.mask "url(#__2vBNHao__mask-16)", fill "#000000", fillRule "evenodd" ]
                                [ rect [ id "__2vBNHao__Rectangle", opacity "0.38", x "0", y "0", width "145", height "32" ]
                                    []
                                ]
                            ]
                        , g [ id "__2vBNHao__Chips-/-Action-/-with-Image-/-Enabled", transform "translate(387.000000, 88.000000)" ]
                            [ mask [ id "__2vBNHao__mask-18", fill "white" ]
                                [ use [ xlinkHref "#__2vBNHao__path-17" ]
                                    []
                                ]
                            , g [ id "__2vBNHao__Surface" ]
                                []
                            , g [ id "__2vBNHao__?-/-Color-/-Icons-/-Black-/-Disabled", opacity "0.119999997", Svg.Attributes.mask "url(#__2vBNHao__mask-18)", fill "#000000", fillRule "evenodd" ]
                                [ rect [ id "__2vBNHao__Rectangle", opacity "0.38", x "0", y "0", width "145", height "32" ]
                                    []
                                ]
                            ]
                        , g [ id "__2vBNHao__Chips-/-Action-/-with-Image-/-Enabled", transform "translate(157.000000, 44.000000)" ]
                            [ mask [ id "__2vBNHao__mask-20", fill "white" ]
                                [ use [ xlinkHref "#__2vBNHao__path-19" ]
                                    []
                                ]
                            , g [ id "__2vBNHao__Surface" ]
                                []
                            , g [ id "__2vBNHao__?-/-Color-/-Icons-/-Black-/-Disabled", opacity "0.119999997", Svg.Attributes.mask "url(#__2vBNHao__mask-20)", fill "#000000", fillRule "evenodd" ]
                                [ rect [ id "__2vBNHao__Rectangle", opacity "0.38", x "0", y "0", width "145", height "32" ]
                                    []
                                ]
                            ]
                        , g [ id "__2vBNHao__Chips-/-Action-/-with-Image-/-Enabled", transform "translate(316.000000, 132.000000)" ]
                            [ mask [ id "__2vBNHao__mask-22", fill "white" ]
                                [ use [ xlinkHref "#__2vBNHao__path-21" ]
                                    []
                                ]
                            , g [ id "__2vBNHao__Surface" ]
                                []
                            , g [ id "__2vBNHao__?-/-Color-/-Icons-/-Black-/-Disabled", opacity "0.119999997", Svg.Attributes.mask "url(#__2vBNHao__mask-22)", fill "#000000", fillRule "evenodd" ]
                                [ rect [ id "__2vBNHao__Rectangle", opacity "0.38", x "0", y "0", width "145", height "32" ]
                                    []
                                ]
                            ]
                        , g [ id "__2vBNHao__Chips-/-Action-/-with-Image-/-Enabled", transform "translate(316.000000, 44.000000)" ]
                            [ mask [ id "__2vBNHao__mask-24", fill "white" ]
                                [ use [ xlinkHref "#__2vBNHao__path-23" ]
                                    []
                                ]
                            , g [ id "__2vBNHao__Surface" ]
                                []
                            , g [ id "__2vBNHao__?-/-Color-/-Icons-/-Black-/-Disabled", opacity "0.119999997", Svg.Attributes.mask "url(#__2vBNHao__mask-24)", fill "#000000", fillRule "evenodd" ]
                                [ rect [ id "__2vBNHao__Rectangle", opacity "0.38", x "0", y "0", width "145", height "32" ]
                                    []
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
