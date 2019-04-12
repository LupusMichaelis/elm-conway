module Controls.Button exposing (button)

import Html as H exposing (Html)
import Html.Events as HE


button :
    msg
    -> String
    -> Html msg
button msg caption =
    H.button
        [ HE.onClick msg
        ]
        [ H.text caption
        ]
