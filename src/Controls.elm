module Controls exposing
    ( DimensionMsg(..)
    , Msg(..)
    , decorate
    , gridCanvas
    , gridDimensioner
    , gridRecycler
    , gridReseter
    , gridSeeders
    , gridSwitch
    )

import Cell
import Controls.Button as CoBu
import Controls.Number as CoNu
import Controls.Selection
import Dimension
import Grid
import Html as H exposing (Html)
import Html.Attributes as HA
import Position
import Seeder
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Svg.Lazy
import Time


type DimensionMsg
    = Height
    | Width


type Msg
    = Resize DimensionMsg Int
    | SelectSeed Controls.Selection.Key
    | RecycleSandbox
    | Reset
    | Tick Time.Posix
    | ToggleRunning



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


gridDimensioner : Dimension.Two -> Html Msg
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
                    Resize dimMsg (String.toInt n |> Maybe.withDefault 0)
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


gridSeeders :
    Controls.Selection.State Msg ( String, Seeder.Type Cell.State )
    -> Html Msg
gridSeeders =
    Controls.Selection.render
        (Controls.Selection.renderElementFromCatalog Seeder.getCatalog (H.text "No name"))
        SelectSeed


gridCanvas : Grid.Grid Cell.State -> Html Msg
gridCanvas grid =
    let
        -- XXX rendering logic should be done somewhere else and tested
        heightPx : String
        heightPx =
            (grid.dimension.h
                * (1 + 10)
                + 1
                |> String.fromInt
            )
                ++ "px"

        widthPx : String
        widthPx =
            (grid.dimension.w
                * (1 + 10)
                + 1
                |> String.fromInt
            )
                ++ "px"

        topPx : Position.Two -> String
        topPx position =
            (position.t
                |> (*) 10
                |> String.fromInt
            )
                ++ "px"

        leftPx : Position.Two -> String
        leftPx position =
            (position.l
                |> (*) 10
                |> String.fromInt
            )
                ++ "px"

        statusToClass : Cell.State -> String
        statusToClass state =
            case state of
                Cell.Live ->
                    "live"

                Cell.Deceased ->
                    "deceased"

        renderPixel : ( Position.Two, Cell.State ) -> Svg Msg
        renderPixel ( position, state ) =
            S.rect
                [ SA.y <| topPx position
                , SA.x <| leftPx position
                , SA.height "10px"
                , SA.width "10px"
                , SA.class <| statusToClass state
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
                |> List.map (Svg.Lazy.lazy renderPixel)
            )
        ]


gridReseter : Html Msg
gridReseter =
    CoBu.button Reset "Reseter"


gridRecycler : Html Msg
gridRecycler =
    CoBu.button RecycleSandbox "Recycle"


gridSwitch : Bool -> Html Msg
gridSwitch running =
    CoBu.button ToggleRunning <|
        if running then
            "Pause"

        else
            "Resume"
