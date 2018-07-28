module Page.Startpage.Svg.Radio exposing (view)

import Html exposing (Html)
import Svg exposing (defs, desc, g, mask, path, polygon, rect, svg, text, title, use)
import Svg.Attributes exposing (d, fill, fillRule, height, id, opacity, points, stroke, strokeWidth, transform, version, viewBox, width, x, xlinkHref, y)


view : Html msg
view =
    svg [ viewBox "0 0 180 180", version "1.1" ]
        [ title []
            [ text "radio_180px" ]
        , desc []
            [ text "Created with Sketch." ]
        , defs []
            [ polygon [ id "__6kMv1Th__path-1", points "0 0 180 0 180 190 0 190" ]
                []
            , path [ d "M12,2 C6.48,2 2,6.48 2,12 C2,17.52 6.48,22 12,22 C17.52,22 22,17.52 22,12 C22,6.48 17.52,2 12,2 Z M12,20 C7.58,20 4,16.42 4,12 C4,7.58 7.58,4 12,4 C16.42,4 20,7.58 20,12 C20,16.42 16.42,20 12,20 Z M12,17 C14.7614237,17 17,14.7614237 17,12 C17,9.23857625 14.7614237,7 12,7 C9.23857625,7 7,9.23857625 7,12 C7,14.7614237 9.23857625,17 12,17 Z", id "__6kMv1Th__path-3" ]
                []
            , path [ d "M12,2 C6.48,2 2,6.48 2,12 C2,17.52 6.48,22 12,22 C17.52,22 22,17.52 22,12 C22,6.48 17.52,2 12,2 Z M12,20 C7.58,20 4,16.42 4,12 C4,7.58 7.58,4 12,4 C16.42,4 20,7.58 20,12 C20,16.42 16.42,20 12,20 Z M12,17 C14.7614237,17 17,14.7614237 17,12 C17,9.23857625 14.7614237,7 12,7 C9.23857625,7 7,9.23857625 7,12 C7,14.7614237 9.23857625,17 12,17 Z", id "__6kMv1Th__path-5" ]
                []
            , path [ d "M12,2 C6.48,2 2,6.48 2,12 C2,17.52 6.48,22 12,22 C17.52,22 22,17.52 22,12 C22,6.48 17.52,2 12,2 Z M12,20 C7.58,20 4,16.42 4,12 C4,7.58 7.58,4 12,4 C16.42,4 20,7.58 20,12 C20,16.42 16.42,20 12,20 Z", id "__6kMv1Th__path-7" ]
                []
            , path [ d "M12,2 C6.48,2 2,6.48 2,12 C2,17.52 6.48,22 12,22 C17.52,22 22,17.52 22,12 C22,6.48 17.52,2 12,2 Z M12,20 C7.58,20 4,16.42 4,12 C4,7.58 7.58,4 12,4 C16.42,4 20,7.58 20,12 C20,16.42 16.42,20 12,20 Z", id "__6kMv1Th__path-9" ]
                []
            ]
        , g [ id "__6kMv1Th__radio_180px", stroke "none", strokeWidth "1", fill "none", fillRule "evenodd" ]
            [ mask [ id "__6kMv1Th__mask-2", fill "white" ]
                [ use [ xlinkHref "#__6kMv1Th__path-1" ]
                    []
                ]
            , use [ id "__6kMv1Th__Mask", fill "#FAFAFA", xlinkHref "#__6kMv1Th__path-1" ]
                []
            , g [ id "__6kMv1Th__Selection-Control-/-Radio-/-On-/-Enabled", Svg.Attributes.mask "url(#__6kMv1Th__mask-2)" ]
                [ g [ transform "translate(78.000000, 27.000000)", id "__6kMv1Th__Icon-/-Toggle-/-Radio-Checked-/-Fill" ]
                    [ g []
                        [ mask [ id "__6kMv1Th__mask-4", fill "white" ]
                            [ use [ xlinkHref "#__6kMv1Th__path-3" ]
                                []
                            ]
                        , g [ id "__6kMv1Th__ic_radio_button_checked_24px", fillRule "nonzero" ]
                            []
                        , g [ id "__6kMv1Th__?-/-Color-/-Icons-/-Black-/-Active", Svg.Attributes.mask "url(#__6kMv1Th__mask-4)", fill "#000000", fillRule "evenodd", opacity "0.87" ]
                            [ rect [ id "__6kMv1Th__Rectangle-13", x "0", y "0", width "24", height "24" ]
                                []
                            ]
                        ]
                    ]
                ]
            , g [ id "__6kMv1Th__Selection-Control-/-Radio-/-On-/-Enabled", Svg.Attributes.mask "url(#__6kMv1Th__mask-2)" ]
                [ g [ transform "translate(78.000000, 95.000000)", id "__6kMv1Th__Icon-/-Toggle-/-Radio-Checked-/-Fill" ]
                    [ g []
                        [ mask [ id "__6kMv1Th__mask-6", fill "white" ]
                            [ use [ xlinkHref "#__6kMv1Th__path-5" ]
                                []
                            ]
                        , g [ id "__6kMv1Th__ic_radio_button_checked_24px", fillRule "nonzero" ]
                            []
                        , g [ id "__6kMv1Th__?-/-Color-/-Icons-/-Black-/-Active", Svg.Attributes.mask "url(#__6kMv1Th__mask-6)", fill "#000000", fillRule "evenodd", opacity "0.87" ]
                            [ rect [ id "__6kMv1Th__Rectangle-13", x "0", y "0", width "24", height "24" ]
                                []
                            ]
                        ]
                    ]
                ]
            , g [ id "__6kMv1Th__Selection-Control-/-Radio-/-Off-/-Enabled", Svg.Attributes.mask "url(#__6kMv1Th__mask-2)" ]
                [ g [ transform "translate(78.000000, 61.000000)", id "__6kMv1Th__Icon-/-Toggle-/-Radio-Unchecked-/-Fill" ]
                    [ g []
                        [ mask [ id "__6kMv1Th__mask-8", fill "white" ]
                            [ use [ xlinkHref "#__6kMv1Th__path-7" ]
                                []
                            ]
                        , g [ id "__6kMv1Th__icon/toggle/radio_button_unchecked_24px", fillRule "nonzero" ]
                            []
                        , g [ id "__6kMv1Th__?-/-Color-/-Icons-/-Black-/-Inactive", Svg.Attributes.mask "url(#__6kMv1Th__mask-8)", fill "#000000", fillRule "evenodd", opacity "0.54" ]
                            [ rect [ id "__6kMv1Th__Rectangle", x "0", y "0", width "24", height "24" ]
                                []
                            ]
                        ]
                    ]
                ]
            , g [ id "__6kMv1Th__Selection-Control-/-Radio-/-Off-/-Enabled", Svg.Attributes.mask "url(#__6kMv1Th__mask-2)" ]
                [ g [ transform "translate(78.000000, 129.000000)", id "__6kMv1Th__Icon-/-Toggle-/-Radio-Unchecked-/-Fill" ]
                    [ g []
                        [ mask [ id "__6kMv1Th__mask-10", fill "white" ]
                            [ use [ xlinkHref "#__6kMv1Th__path-9" ]
                                []
                            ]
                        , g [ id "__6kMv1Th__icon/toggle/radio_button_unchecked_24px", fillRule "nonzero" ]
                            []
                        , g [ id "__6kMv1Th__?-/-Color-/-Icons-/-Black-/-Inactive", Svg.Attributes.mask "url(#__6kMv1Th__mask-10)", fill "#000000", fillRule "evenodd", opacity "0.54" ]
                            [ rect [ id "__6kMv1Th__Rectangle", x "0", y "0", width "24", height "24" ]
                                []
                            ]
                        ]
                    ]
                ]
            ]
        ]
