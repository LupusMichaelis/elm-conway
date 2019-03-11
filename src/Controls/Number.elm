module Controls.Number exposing
    ( ctrl
    )

import Controls.Button as CoBu
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE


type alias Model =
    { value : Int }


ctrl :
    (String -> msg)
    -> msg
    -> msg
    -> String
    -> Model
    -> Html msg
ctrl change decrease increase label model =
    H.label []
        [ H.text label
        , H.input
            [ HA.value <| toString model.value
            , HE.onInput change
            ]
            []
        , CoBu.button increase "↑"
        , CoBu.button decrease "↓"
        ]
