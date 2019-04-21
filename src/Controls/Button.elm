module Controls.Button exposing (button)

import Html as H exposing (Html)
import Html.Events as HE


button :
    msg
    -> String
    -> Html msg
button msg =
    H.text
        >> List.singleton
        >> H.button
            [ HE.onClick msg
            ]
