module Controls exposing
    ( Msg(..)
    , decorate
    , gridCanvas
    , gridDimensioner
    , gridRecycler
    , gridReseter
    , gridSeeders
    )

import Array exposing (Array)
import Grid exposing (Grid)
import Grid.Cell
import Controls.Button as CoBu
import Controls.Number as CoNu
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
    | SelectSeed Int
    | RecycleSandbox
    | Reset
    | Tick Time.Time -- XXX separate concerns



-- hack to load CSS


decorate : Html Msg
decorate =
    H.node
        "link"
        [ HA.href "/assets/grid.css"
        , HA.rel "stylesheet"
        , HA.type_ "text/css"
        ]
        []


gridDimensioner : Grid.Dimension -> Html Msg
gridDimensioner dim =
    H.div []
        [ CoNu.ctrl
            ChangeHeight
            IncreaseHeight
            DecreaseHeight
            "Height:"
            { value = dim.h }
        , CoNu.ctrl
            ChangeWidth
            IncreaseWidth
            DecreaseWidth
            "Width:"
            { value = dim.w }
        ]


gridSeeders : Int -> Array ( String, Int -> Grid.Cell.State ) -> Html Msg
gridSeeders current seeders =
    let
        liClass : Int -> String
        liClass idx =
            if idx == current then
                "selected"

            else
                ""
    in
    H.div []
        [ H.ul []
            (Array.map Tuple.first seeders
                |> Array.indexedMap
                    (\i t ->
                        H.li
                            [ HE.onClick (SelectSeed i)
                            , HA.class (liClass i)
                            ]
                            [ H.text t ]
                    )
                |> Array.toList
            )
        ]


gridCanvas : Grid.Grid -> Html Msg
gridCanvas grid =
    let
        -- XXX rendering logic should be done somewhere else and tested
        heightPx : String
        heightPx =
            (grid.dimension.h
                * (1 + 10)
                + 1
                |> toString
            )
                ++ "px"

        widthPx : String
        widthPx =
            (grid.dimension.w
                * (1 + 10)
                + 1
                |> toString
            )
                ++ "px"

        topPx : Grid.Position -> String
        topPx position =
            (position.t
                |> (*) 10
                |> toString
            )
                ++ "px"

        leftPx : Grid.Position -> String
        leftPx position =
            (position.l
                |> (*) 10
                |> toString
            )
                ++ "px"

        statusToClass : Grid.Cell.State -> String
        statusToClass state =
            case state of
                Grid.Cell.Live ->
                    "live"

                Grid.Cell.Deceased ->
                    "deceased"

        renderPixel : ( Grid.Position, Grid.Cell.State ) -> Svg Msg
        renderPixel ( position, state ) =
            S.rect
                [ SA.x (topPx position)
                , SA.y (leftPx position)
                , SA.height "10px"
                , SA.width "10px"
                , SA.class (statusToClass state)
                ]
                []
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
                |> Array.toList
            )
        ]


gridReseter : Html Msg
gridReseter =
    CoBu.button Reset "Reseter"


gridRecycler : Html Msg
gridRecycler =
    CoBu.button RecycleSandbox "Recycle"
