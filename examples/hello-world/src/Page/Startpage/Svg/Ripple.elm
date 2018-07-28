module Page.Startpage.Svg.Ripple exposing (view)

import Html exposing (Html)
import Svg exposing (defs, desc, feColorMatrix, feGaussianBlur, feMerge, feMergeNode, feOffset, filter, g, mask, path, rect, svg, text, title, use)
import Svg.Attributes exposing (d, dx, dy, fill, fillOpacity, fillRule, filterUnits, height, id, in_, opacity, result, rx, stdDeviation, stroke, strokeWidth, transform, type_, values, version, viewBox, width, x, xlinkHref, y)


view : Html msg
view =
    svg [ viewBox "0 0 180 180", version "1.1" ]
        [ title []
            [ text "ripple_180px" ]
        , desc []
            [ text "Created with Sketch." ]
        , defs []
            [ rect [ id "__3FsO9xo__path-1", x "0", y "0", width "180", height "180" ]
                []
            , path [ d "M4,0 L120,0 C122.209139,-4.05812251e-16 124,1.790861 124,4 L124,120 C124,122.209139 122.209139,124 120,124 L4,124 C1.790861,124 2.705415e-16,122.209139 0,120 L0,4 C-2.705415e-16,1.790861 1.790861,4.05812251e-16 4,0 Z", id "__3FsO9xo__path-3" ]
                []
            , filter [ x "-19.8%", y "-14.1%", width "139.5%", height "139.5%", filterUnits "objectBoundingBox", id "__3FsO9xo__filter-5" ]
                [ feOffset [ dx "0", dy "1", in_ "SourceAlpha", result "shadowOffsetOuter1" ]
                    []
                , feGaussianBlur [ stdDeviation "5", in_ "shadowOffsetOuter1", result "shadowBlurOuter1" ]
                    []
                , feColorMatrix [ values "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.2 0", type_ "matrix", in_ "shadowBlurOuter1", result "shadowMatrixOuter1" ]
                    []
                , feOffset [ dx "0", dy "4", in_ "SourceAlpha", result "shadowOffsetOuter2" ]
                    []
                , feGaussianBlur [ stdDeviation "2.5", in_ "shadowOffsetOuter2", result "shadowBlurOuter2" ]
                    []
                , feColorMatrix [ values "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.12 0", type_ "matrix", in_ "shadowBlurOuter2", result "shadowMatrixOuter2" ]
                    []
                , feOffset [ dx "0", dy "2", in_ "SourceAlpha", result "shadowOffsetOuter3" ]
                    []
                , feGaussianBlur [ stdDeviation "2", in_ "shadowOffsetOuter3", result "shadowBlurOuter3" ]
                    []
                , feColorMatrix [ values "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.14 0", type_ "matrix", in_ "shadowBlurOuter3", result "shadowMatrixOuter3" ]
                    []
                , feMerge []
                    [ feMergeNode [ in_ "shadowMatrixOuter1" ]
                        []
                    , feMergeNode [ in_ "shadowMatrixOuter2" ]
                        []
                    , feMergeNode [ in_ "shadowMatrixOuter3" ]
                        []
                    ]
                ]
            ]
        , g [ id "__3FsO9xo__ripple_180px", stroke "none", strokeWidth "1", fill "none", fillRule "evenodd" ]
            [ g [ id "__3FsO9xo__Cards-/-Elements-/-Surface-/-Pressed" ]
                [ mask [ id "__3FsO9xo__mask-2", fill "white" ]
                    [ use [ xlinkHref "#__3FsO9xo__path-1" ]
                        []
                    ]
                , use [ id "__3FsO9xo__Mask", fill "#FAFAFA", xlinkHref "#__3FsO9xo__path-1" ]
                    []
                , g [ Svg.Attributes.mask "url(#__3FsO9xo__mask-2)" ]
                    [ g [ transform "translate(28.000000, 28.000000)" ]
                        [ mask [ id "__3FsO9xo__mask-4", fill "white" ]
                            [ use [ xlinkHref "#__3FsO9xo__path-3" ]
                                []
                            ]
                        , g [ id "__3FsO9xo__Surface", stroke "none", fill "none" ]
                            [ use [ fill "black", fillOpacity "1", Svg.Attributes.filter "url(#__3FsO9xo__filter-5)", xlinkHref "#__3FsO9xo__path-3" ]
                                []
                            , use [ fill "#FAFAFA", fillRule "evenodd", xlinkHref "#__3FsO9xo__path-3" ]
                                []
                            ]
                        , g [ id "__3FsO9xo__?-/-Color-/-Surface-/-Light", stroke "none", fill "none", Svg.Attributes.mask "url(#__3FsO9xo__mask-4)", fillRule "evenodd" ]
                            [ rect [ id "__3FsO9xo__Rectangle-13", fill "#FAFAFA", x "0", y "0", width "124", height "124" ]
                                []
                            ]
                        , g [ id "__3FsO9xo__?-/-Ripple-/-Black-/-Pressed", stroke "none", fill "none", Svg.Attributes.mask "url(#__3FsO9xo__mask-4)", fillRule "evenodd", opacity "0.16" ]
                            [ g [ transform "translate(24.473684, 11.094737)", fill "#000000", id "__3FsO9xo__Black" ]
                                [ rect [ x "0", y "0", width "119", height "119", rx "59.5" ]
                                    []
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
