module Controls.Number exposing
    ( Msg(..)
    , State
    , ctrl
    , init
    , update
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


update : Msg -> State -> State
update msg model =
    case msg of
        Decrease ->
            { model | value = model.value - 1 }

        Increase ->
            { model | value = model.value + 1 }

        Change s ->
            { model | value = String.toInt s |> Result.withDefault 0 }


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
