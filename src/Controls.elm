module Controls exposing
    ( DimensionMsg(..)
    , Msg(..)
    , Speed(..)
    , decorate
    , gridCanvas
    , gridDimensioner
    , gridRecycler
    , gridReseter
    , gridSeeders
    , gridSlowDown
    , gridSpeedUp
    , gridSwitch
    )

import Cell
import Controls.Button
import Controls.Number
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


type Speed
    = Slow
    | Normal
    | Fast


type Msg
    = Resize DimensionMsg Int
    | SelectSeed Controls.Selection.Key
    | RecycleSandbox
    | Reset
    | Tick Time.Posix
    | ToggleRunning
    | SlowDown
    | SpeedUp



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

        toMsg : DimensionMsg -> Controls.Number.Msg -> Msg
        toMsg dimMsg msg =
            case msg of
                Controls.Number.Increase ->
                    Resize dimMsg (dimMsgToInt dimMsg + 1)

                Controls.Number.Decrease ->
                    Resize dimMsg (dimMsgToInt dimMsg - 1)

                Controls.Number.Change n ->
                    Resize dimMsg (String.toInt n |> Maybe.withDefault 0)
    in
    H.div []
        [ Controls.Number.ctrl
            (toMsg Height << Controls.Number.Change)
            (toMsg Height Controls.Number.Increase)
            (toMsg Height Controls.Number.Decrease)
            "Height:"
            (Controls.Number.init dim.h)
        , Controls.Number.ctrl
            (toMsg Width << Controls.Number.Change)
            (toMsg Width Controls.Number.Increase)
            (toMsg Width Controls.Number.Decrease)
            "Width:"
            (Controls.Number.init dim.w)
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
    Controls.Button.button Reset "Reseter"


gridRecycler : Html Msg
gridRecycler =
    Controls.Button.button RecycleSandbox "Recycle"


gridSwitch : Bool -> Html Msg
gridSwitch running =
    Controls.Button.button ToggleRunning <|
        if running then
            "Pause"

        else
            "Resume"


gridSpeedUp : Speed -> Html Msg
gridSpeedUp speed =
    Controls.Button.button SpeedUp
        (case speed of
            Slow ->
                "Normal"

            Normal ->
                "Fast"

            Fast ->
                "Fast"
        )


gridSlowDown : Speed -> Html Msg
gridSlowDown speed =
    Controls.Button.button SlowDown
        (case speed of
            Slow ->
                "Slow"

            Normal ->
                "Slow"

            Fast ->
                "Normal"
        )
