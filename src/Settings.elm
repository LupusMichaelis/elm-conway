module Settings exposing
    ( Type
    , get
    )

import Cell
import Controls
import Controls.Canvas
import Dimension
import List.Nonempty


type alias Type =
    { canvas : Controls.Canvas.Type Cell.State
    , gridDimension : Dimension.Two
    , speedList : List.Nonempty.Nonempty ( Controls.Speed, Int )
    }


get : Type
get =
    { canvas =
        Controls.Canvas.Type
            1
            8
            (\state ->
                case state of
                    Cell.Live ->
                        "live"

                    Cell.Deceased ->
                        "deceased"
            )
            Controls.Canvas.Rectangle
    , gridDimension =
        Dimension.make 33 33
    , speedList =
        List.Nonempty.Nonempty
            ( Controls.Slow, 1000 )
            [ ( Controls.Normal, 500 )
            , ( Controls.Fast, 200 )
            ]
    }
