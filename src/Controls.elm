module Controls exposing
    ( Msg

    , decorate

    , gridDimensioner
    , gridSeeders
    , gridCanvas
    , gridReseter
    )

import Grid exposing (Grid)
import Grid.Cell

import Array exposing (Array)

import Html as H exposing (Html)
import Html.Attributes as HA

import Svg as S exposing (Svg)
import Svg.Attributes as SA

import Tuple

type Msg
    = ReduceHeight
    | RaiseHeight
    | ReduceWidth
    | RaiseWidth
    | SelectSeed
    | ResetSandbox

-- hack to load CSS
decorate: Html Msg
decorate =
    H.node
        "link"
        [ HA.href   "/assets/grid.css"
        , HA.rel    "stylesheet"
        , HA.type_  "text/css"
        ]
        []

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

gridCanvas: Grid.Grid -> Html Msg
gridCanvas grid =
    let
        -- XXX rendering logic should be done somewhere else and tested
        heightPx: String
        heightPx =
            (grid.dimension.h * ( 1 + 10 ) + 1
                |> toString) ++ "px"

        widthPx: String
        widthPx =
            (grid.dimension.w * ( 1 + 10 ) + 1
                |> toString) ++ "px"

        topPx: Grid.Position -> String
        topPx position =
            (position.t
                |> (*) 10
                |> toString) ++ "px"

        leftPx: Grid.Position -> String
        leftPx position =
            (position.l
                |> (*) 10
                |> toString) ++ "px"

        statusToClass: Grid.Cell.State -> String
        statusToClass state =
            case state of
                Grid.Cell.Empty ->
                    "empty"
                Grid.Cell.Live ->
                    "live"
                Grid.Cell.Deceased ->
                    "deceased"

        renderPixel: (Grid.Position, Grid.Cell.State) -> Svg Msg
        renderPixel (position, state) =
            S.rect
                [ SA.x (topPx position)
                , SA.y (leftPx position)
                , SA.height "10px"
                , SA.width "10px"
                , SA.class (statusToClass state)
                ]
                [ ]

    in
        H.div []
            [ S.svg
                [ SA.height heightPx
                , SA.width widthPx
                , SA.fill "black"
                , SA.stroke "white"
                , SA.strokeWidth "1px"
                ]
                (Grid.iterate grid
                    |> Array.map renderPixel
                    |> Array.toList)
            ]

gridReseter: Html Msg
gridReseter =
    H.div [] [H.text "Reseter"]
