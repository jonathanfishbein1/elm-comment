module CommentStyles exposing (..)

import Style
    exposing
        ( Property
        , StyleSheet
        , opacity
        , prop
        , style
        )
import Style.Font
    exposing
        ( center
        , font
        , lineHeight
        , size
        , typeface
        , uppercase
        )


type Class
    = None
    | Paragraph


typefaceStack : Property class variation
typefaceStack =
    typeface [ font "Josefin Sans", font "Calibri", font "Helvetica", font "sans-serif" ]


stylesheet : String -> StyleSheet Class variation
stylesheet heroBackgroundImage =
    Style.styleSheet
        [ style None []
        , style Paragraph
            [ lineHeight 1.6
            , typefaceStack
            ]
        ]
