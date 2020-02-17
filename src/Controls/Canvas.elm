module Controls.Canvas exposing
    ( Shape(..)
    , Type
    , getCatalogOfShape
    , getCatalogOfShapeEntry
    , getDefaultShape
    , getDefaultShapeKey
    , getDefaultShapeValue
    , node
    )

import Array
import Basic
import Dict exposing (Dict)
import Dict.Nonempty
import Grid
import Html as H exposing (Html)
import Html.Attributes as HA
import List.Nonempty
import Position
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Svg.Lazy


type alias Type state =
    { gap : Int
    , side : Int
    , stateToClass : state -> String
    , shape : Shape
    }


type Shape
    = Rectangle
    | Circle


getCatalogOfShapeEntry : Shape -> Maybe ( Int, ( String, Shape ) )
getCatalogOfShapeEntry shape =
    getCatalogOfShape
        |> Dict.Nonempty.toList
        |> List.filter (Tuple.second >> Tuple.second >> (==) shape)
        |> List.map Tuple.first
        |> List.map (\key -> ( key, getCatalogOfShape |> Dict.Nonempty.get key ))
        |> List.map (Tuple.mapFirst Just)
        |> List.map (Basic.uncurry (Maybe.map2 Tuple.pair))
        |> List.filterMap identity
        |> List.head


getCatalogOfShape : Dict.Nonempty.Nonempty Int ( String, Shape )
getCatalogOfShape =
    List.Nonempty.Nonempty
        ( 0, ( "Rectangle", Rectangle ) )
        [ ( 1, ( "Circle", Circle ) ) ]
        |> Dict.Nonempty.fromNonemptyList


getDefaultShape : ( Int, ( String, Shape ) )
getDefaultShape =
    getCatalogOfShape
        |> Dict.Nonempty.get 1
        |> Maybe.map (Tuple.pair 1)
        |> Maybe.withDefault ( 0, ( "Rectangle", Rectangle ) )


getDefaultShapeValue : ( String, Shape )
getDefaultShapeValue =
    getDefaultShape |> Tuple.second


getDefaultShapeKey : Int
getDefaultShapeKey =
    getDefaultShape |> Tuple.first


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
                |> Array.map (Svg.Lazy.lazy (renderPixel canvas grid))
                |> Array.toList
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
    case canvas.shape of
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
