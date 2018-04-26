module OnClickPreventDefault exposing (onClickPreventDefault)

import Html
import Html.Events
    exposing
        ( onWithOptions
        )
import Json.Decode exposing (succeed)


onClickPreventDefault : msg -> Html.Attribute msg
onClickPreventDefault message =
    onWithOptions "click"
        { stopPropagation = False
        , preventDefault = True
        }
        (succeed message)
