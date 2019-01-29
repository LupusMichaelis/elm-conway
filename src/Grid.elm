module Grid exposing
    -- types defined here
    ( CellState(..)
    , Grid

    -- types imported exposed
    , Dimension
    , Position

    -- functions defined here
    , isWithinDimension
    , getStateAt
    , getNeighbourPositions
    , make
    , makeDimension
    , makeFromList
    , makePosition
    , positionToFlat
    , positionFromFlat
    )

import Grid.Dimension exposing (Dimension)
import Grid.Position exposing (Position)

import Array exposing (Array)

type CellState =
    Live            -- happy cell shanting around
    | Deceased      -- a corpse's lying there
    | Empty         -- no live cell's present

type alias Dimension = Grid.Dimension.Dimension
type alias Position = Grid.Position.Position

type alias Grid =
    { dimension: Dimension
    , flatten: Array CellState
    }

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

isWithinDimension: Dimension -> Position -> Bool
isWithinDimension d p =
    p.t >= 0
        && p.l >= 0
        && d.h > p.t
        && d.w > p.l

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
        positionToFlat grid.dimension position
            |> Maybe.andThen
                (\pos -> Array.get
                    pos
                    grid.flatten
                )
            |> Maybe.withDefault Empty

-- that's BS, should be able to make that with a turtle and distance mesurement
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

positionToFlat: Dimension -> Position -> Maybe Int
positionToFlat dim pos =
    if pos.t >= dim.h || pos.l >= dim.w then
        Nothing
    else
        Just (pos.t * dim.h + pos.l)

positionFromFlat: Dimension -> Int -> Maybe Position
positionFromFlat dim flat =
    if flat < Grid.Dimension.getArea dim then
        Just (Position
                (flat // dim.h)
                (flat % dim.h))
    else
        Nothing
