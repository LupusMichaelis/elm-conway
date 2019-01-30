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

import Html exposing
    ( Html
    , button
    , div
    , input
    , text
    , label
    , li
    , ul
    )

import Html.Attributes exposing
    ( value
    )

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
    label []
        [ text l
        , input [value <| toString n] []
        , button [] [ text "↑" ]
        , button [] [ text "↓" ]
        ]

gridDimensioner: Grid.Dimension -> Html Msg
gridDimensioner dim =
    div []
        [ numberController "Height:" dim.h
        , numberController "Width:" dim.w
        ]

gridSeeders: Int -> Array (String, Int -> Grid.Cell.State) -> Html Msg
gridSeeders current seeders =
    div []
        [ ul []
            (Array.map Tuple.first seeders
                |> Array.map (\t -> li [] [text t])
                |> Array.toList)
        ]

gridRenderer: Grid.Grid -> Html Msg
gridRenderer grid =
    div [] [text "Renderer"]

gridReseter: Html Msg
gridReseter =
    div [] [text "Reseter"]
