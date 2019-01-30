module Controls exposing
    ( Msg

    , gridDimensioner
    , gridSeeders
    , gridRenderer
    , gridReseter
    )

import Grid exposing (Grid)
import Grid.Cell

import Array exposing (Array)

import Html as H exposing (Html)
import Html.Attributes as HA


import Tuple

type Msg
    = ReduceHeight
    | RaiseHeight
    | ReduceWidth
    | RaiseWidth
    | SelectSeed
    | ResetSandbox

numberController: String -> number -> Html Msg
numberController l n =
    H.label []
        [ H.text l
        , H.input [HA.value <| toString n] []
        , H.button [] [ H.text "↑" ]
        , H.button [] [ H.text "↓" ]
        ]

gridDimensioner: Grid.Dimension -> Html Msg
gridDimensioner dim =
    H.div []
        [ numberController "Height:" dim.h
        , numberController "Width:" dim.w
        ]

gridSeeders: Int -> Array (String, Int -> Grid.Cell.State) -> Html Msg
gridSeeders current seeders =
    H.div []
        [ H.ul []
            (Array.map Tuple.first seeders
                |> Array.map (\t -> H.li [] [H.text t])
                |> Array.toList)
        ]

gridRenderer: Grid.Grid -> Html Msg
gridRenderer grid =
    div [] [text "Renderer"]

gridReseter: Html Msg
gridReseter =
    H.div [] [H.text "Reseter"]
