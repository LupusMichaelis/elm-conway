module Controls exposing
    ( Msg

    , gridDimensioner
    , gridSeeder
    , gridRenderer
    , gridReseter
    )

import Grid exposing (Grid)

import Html exposing
    ( Html
    , button
    , div
    , input
    , text
    , label
    )

import Html.Attributes exposing
    ( value
    )

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

gridSeeder: Html Msg
gridSeeder =
    div [] [text "Seeder"]

gridRenderer: Grid.Grid -> Html Msg
gridRenderer grid =
    div [] [text "Renderer"]

gridReseter: Html Msg
gridReseter =
    div [] [text "Reseter"]
