module Controls exposing
    ( Msg
    , Msg(..)

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
import Html.Events as HE
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Time
import Tuple

type Msg
    = DecreaseHeight
    | IncreaseHeight
    | ChangeHeight String
    | DecreaseWidth
    | IncreaseWidth
    | ChangeWidth String
    | SelectSeed -- element idx??? XXX
    | ResetSandbox
    | Tick Time.Time -- XXX separate concerns

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


gridDimensioner: Grid.Dimension -> Html Msg
gridDimensioner dim =
    H.div []
        [ H.label []
            [ H.text "Height:"
            , H.input
                [ HA.value <| toString dim.h
                , HE.onInput ChangeHeight
                ]
                []
            , H.button
                [ HE.onClick IncreaseHeight
                ]
                [ H.text "↑" ]
            , H.button
                [ HE.onClick DecreaseHeight
                ]
                [ H.text "↓" ]
            ]
            , H.label []
                [ H.text "Width:"
                , H.input
                    [ HA.value <| toString dim.w
                    , HE.onInput ChangeWidth
                    ]
                    []
                , H.button
                    [ HE.onClick IncreaseWidth
                    ]
                    [ H.text "↑" ]
                , H.button
                    [ HE.onClick DecreaseWidth
                    ]
                    [ H.text "↓" ]
                ]
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
    H.button
        [ HE.onClick ResetSandbox
        ]
        [ H.text "Reseter"
        ]
