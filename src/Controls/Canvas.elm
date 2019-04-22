module Controls.Canvas exposing
    ( Kind(..)
    , Type
    , node
    )

import Grid
import Html as H exposing (Html)
import Html.Attributes as HA
import Position
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Svg.Lazy


type alias Type state =
    { gap : Int
    , side : Int
    , stateToClass : state -> String
    , kind : Kind
    }


type Kind
    = Rectangle
    | Circle


pixel : Int -> String
pixel n =
    String.fromInt n ++ "px"


node : Type state -> Grid.Grid state -> Html msg
node canvas grid =
    H.div []
        [ S.svg
            [ SA.height <| pixel <| height canvas grid
            , SA.width <| pixel <| width canvas grid
            , SA.fill "black"
            , SA.stroke "white"
            , SA.strokeWidth <| pixel canvas.gap
            ]
            (Grid.iterate grid
                |> List.map (Svg.Lazy.lazy (renderPixel canvas grid))
            )
        ]


height : Type state -> Grid.Grid state -> Int
height canvas grid =
    grid.dimension.h
        * (canvas.gap + canvas.side)
        + canvas.gap


width : Type state -> Grid.Grid state -> Int
width canvas grid =
    grid.dimension.w
        * (canvas.gap + canvas.side)
        + canvas.gap


top : Type state -> Position.Two -> Int
top canvas position =
    position.t * canvas.side


left : Type state -> Position.Two -> Int
left canvas position =
    position.l * canvas.side


renderPixel : Type state -> Grid.Grid state -> ( Position.Two, state ) -> Svg msg
renderPixel canvas =
    case canvas.kind of
        Rectangle ->
            renderPixelAsRectangle canvas

        Circle ->
            renderPixelAsCircle canvas


renderPixelAsRectangle : Type state -> Grid.Grid state -> ( Position.Two, state ) -> Svg msg
renderPixelAsRectangle canvas grid ( position, state ) =
    S.rect
        [ SA.y <| pixel <| top canvas position
        , SA.x <| pixel <| left canvas position
        , SA.height <| pixel canvas.side
        , SA.width <| pixel canvas.side
        , SA.class <| canvas.stateToClass state
        ]
        []


renderPixelAsCircle : Type state -> Grid.Grid state -> ( Position.Two, state ) -> Svg msg
renderPixelAsCircle canvas grid ( position, state ) =
    S.circle
        [ SA.cy <| pixel <| top canvas position + canvas.side // 2
        , SA.cx <| pixel <| left canvas position + canvas.side // 2
        , SA.r <| pixel <| canvas.side // 2
        , SA.class <| canvas.stateToClass state
        ]
        []
