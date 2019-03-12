module Controls.Number exposing
    ( Msg(..)
    , State
    , ctrl
    , init
    )

import Controls.Button as CoBu
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE


type Msg
    = Decrease
    | Increase
    | Change String


type alias State =
    { value : Int }


init : Int -> State
init v =
    { value = v }


ctrl :
    (String -> msg)
    -> msg
    -> msg
    -> String
    -> State
    -> Html msg
ctrl change increase decrease label model =
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
