module Controls exposing
    ( DimensionMsg(..)
    , Msg(..)
    , decorate
    , gridCanvas
    , gridDimensioner
    , gridRecycler
    , gridReseter
    , gridSeeders
    )

import Array exposing (Array)
import Controls.Button as CoBu
import Controls.Number as CoNu
import Grid exposing (Grid)
import Grid.Cell
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Time
import Tuple


type DimensionMsg
    = Height
    | Width


type Msg
    = Resize DimensionMsg Int
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
    let
        dimMsgToInt : DimensionMsg -> Int
        dimMsgToInt dimMsg =
            case dimMsg of
                Width ->
                    dim.w

                Height ->
                    dim.h

        toMsg : DimensionMsg -> CoNu.Msg -> Msg
        toMsg dimMsg msg =
            case msg of
                CoNu.Increase ->
                    Resize dimMsg (dimMsgToInt dimMsg + 1)

                CoNu.Decrease ->
                    Resize dimMsg (dimMsgToInt dimMsg - 1)

                CoNu.Change n ->
                    Resize dimMsg (String.toInt n |> Result.withDefault 0)
    in
    H.div []
        [ CoNu.ctrl
            (toMsg Height << CoNu.Change)
            (toMsg Height CoNu.Increase)
            (toMsg Height CoNu.Decrease)
            "Height:"
            (CoNu.init dim.h)
        , CoNu.ctrl
            (toMsg Width << CoNu.Change)
            (toMsg Width CoNu.Increase)
            (toMsg Width CoNu.Decrease)
            "Width:"
            (CoNu.init dim.w)
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
