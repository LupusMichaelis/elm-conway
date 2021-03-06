module Controls exposing
    ( DimensionMsg(..)
    , Msg(..)
    , Speed(..)
    , decorate
    , gridCanvas
    , gridDimensioner
    , gridRecycler
    , gridReseter
    , gridSelectSeeder
    , gridSelectShape
    , gridSlowDown
    , gridSpeedUp
    , gridSwitch
    )

import Cell
import Controls.Button
import Controls.Canvas
import Controls.Number
import Controls.Selection
import Dimension
import Grid
import Html as H exposing (Html)
import Html.Attributes as HA
import Position
import Seeder
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
    | SelectShape Controls.Selection.Key
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
                    dimMsgToInt dimMsg
                        + 1
                        |> Resize dimMsg

                Controls.Number.Decrease ->
                    dimMsgToInt dimMsg
                        - 1
                        |> Resize dimMsg

                Controls.Number.Change n ->
                    String.toInt n
                        |> Maybe.withDefault 0
                        |> Resize dimMsg
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


gridSelectShape :
    Controls.Selection.State Msg ( String, Controls.Canvas.Shape )
    -> Html Msg
gridSelectShape =
    Controls.Selection.render
        (Controls.Selection.renderElementFromCatalog
            Controls.Canvas.getCatalogOfShape
            (H.text "No name")
        )
        SelectShape


gridSelectSeeder :
    Controls.Selection.State Msg ( String, Seeder.Type Cell.State )
    -> Html Msg
gridSelectSeeder =
    let
        catalogEntryRenderer =
            Controls.Selection.renderElementFromCatalog
                Seeder.getCatalog
            <|
                H.text "Anonymous seeder"
    in
    Controls.Selection.render
        catalogEntryRenderer
        SelectSeed


gridCanvas :
    Controls.Canvas.Type Cell.State
    -> Grid.Grid Cell.State
    -> Html Msg
gridCanvas =
    Controls.Canvas.node


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
