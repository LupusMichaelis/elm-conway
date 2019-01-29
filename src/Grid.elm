module Grid exposing
    -- types defined in this module
    ( CellState
    , Grid

    -- exposing imported types
    , Dimension
    , Position

    -- functions defined in this module
    , convertPositionToFlat
    , convertPositionFromFlat
    , isWithinDimension
    , getStateAt
    , getNeighbourPositions
    , make
    , makeDimension
    , makeFromList
    , makePosition
    )

import Grid.Cell exposing (State(..))
import Grid.Dimension exposing (Dimension)
import Grid.Position exposing (Position)

import Array exposing (Array)

type alias CellState = Grid.Cell.State
type alias Dimension = Grid.Dimension.Dimension
type alias Position = Grid.Position.Position

type alias Grid =
    { dimension: Dimension
    , flatten: Array CellState
    }

-- Factories

makeDimension: Int -> Int -> Dimension
makeDimension = Grid.Dimension.make

makePosition: Int -> Int -> Position
makePosition = Grid.Position.make

make: Dimension -> (Int -> CellState) -> Grid
make dim gen =
    Grid
        (dim)
        (Array.initialize
            ( dim.w * dim.h) gen)

makeFromList: Dimension -> List CellState -> Maybe Grid
makeFromList dim list =
    if Grid.Dimension.getArea dim /= List.length list then
        Nothing
    else
        Just <| Grid dim <| Array.fromList list


-- Manipulating positions within dimensions

isWithinDimension: Dimension -> Position -> Bool
isWithinDimension d p =
    p.t >= 0
        && p.l >= 0
        && d.h > p.t
        && d.w > p.l

-- that's BS, should be able to make that with a turtle and distance mesurement
{-| Given a Dimension and a Position, returns an Array of neighbouring positions within
    the borders.
--}
getNeighbourPositions: Dimension -> Position -> Array Position
getNeighbourPositions dim p =
    [ { p | t = p.t - 1, l = p.l - 1 }
    , { p | t = p.t - 1 }
    , { p | t = p.t - 1, l = p.l + 1 }

    , { p | t = p.t, l = p.l - 1 }
    , { p | t = p.t, l = p.l + 1 }

    , { p | t = p.t + 1, l = p.l - 1 }
    , { p | t = p.t + 1 }
    , { p | t = p.t + 1, l = p.l + 1 }
    ]
        |> List.filter (isWithinDimension dim)
        |> Array.fromList


-- Conversion helpers

convertPositionToFlat: Dimension -> Position -> Maybe Int
convertPositionToFlat dim pos =
    if pos.t >= dim.h || pos.l >= dim.w then
        Nothing
    else
        Just (pos.t * dim.h + pos.l)

convertPositionFromFlat: Dimension -> Int -> Maybe Position
convertPositionFromFlat dim flat =
    if flat < Grid.Dimension.getArea dim then
        Just (Position
                (flat // dim.h)
                (flat % dim.h))
    else
        Nothing


-- Inspect grid

getStateAt: Grid -> Position -> CellState
getStateAt grid position =
    let
        maybePosition: Maybe Position
        maybePosition =
            if isWithinDimension grid.dimension position then
                Just position
            else
                Nothing

    in
        convertPositionToFlat grid.dimension position
            |> Maybe.andThen
                (\pos -> Array.get
                    pos
                    grid.flatten
                )
            |> Maybe.withDefault Empty
