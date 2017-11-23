module CommentStyles
    exposing
        ( Class(..)
        , stylesheet
        )

{-| This library is a comments library similar to that found on social media sites.

@docs Class, stylesheet

-}

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


{-| Comments style classes
-}
type Class
    = None
    | Paragraph


typefaceStack : Property class variation
typefaceStack =
    typeface [ font "Josefin Sans", font "Calibri", font "Helvetica", font "sans-serif" ]


{-| Comments stylesheet
-}
stylesheet : String -> StyleSheet Class variation
stylesheet heroBackgroundImage =
    Style.styleSheet
        [ style None []
        , style Paragraph
            [ lineHeight 1.6
            , typefaceStack
            ]
        ]
