module OnClickPreventDefault exposing (onClickPreventDefault)

import Element exposing (Attribute)
import Element.Events
    exposing
        ( onWithOptions
        )
import Json.Decode exposing (succeed)


onClickPreventDefault : msg -> Attribute variation msg
onClickPreventDefault message =
    onWithOptions "click"
        { stopPropagation = False
        , preventDefault = True
        }
        (succeed message)
