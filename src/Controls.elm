module Controls exposing
    ( Msg

    , gridDimensioner
    , gridSeeder
    , gridRenderer
    , gridReseter
    )

import Html exposing (Html, div, text)

type Msg = Nothing

gridDimensioner: Html Msg
gridDimensioner =
    div [] [text "Dimensioner"]

gridSeeder: Html Msg
gridSeeder =
    div [] [text "Seeder"]

gridRenderer: Html Msg
gridRenderer =
    div [] [text "Renderer"]

gridReseter: Html Msg
gridReseter =
    div [] [text "Reseter"]
