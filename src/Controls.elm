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
gridCanvas =
    Controls.Canvas.node <|
        Controls.Canvas.Type
            1
            16
            (\state ->
                case state of
                    Cell.Live ->
                        "live"

                    Cell.Deceased ->
                        "deceased"
            )
            Controls.Canvas.Circle


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
