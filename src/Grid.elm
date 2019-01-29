module Grid exposing
    ( CellState(..)
    , Dimension
    , Grid
    , Position

    , isWithinDimension
    , getStateAt
    , getNeighbourPositions
    , make
    , makeDimension
    , makeFromList
    , makePosition
    )

import Array exposing (Array)

type CellState =
    Live            -- happy cell shanting around
    | Deceased      -- a corpse's lying there
    | Empty         -- no live cell's present

type alias Dimension =
    { h: Int    -- height
    , w: Int    -- width
    }

type alias Position =
    { t: Int    -- from top
    , l: Int    -- from left
    }

type alias Grid =
    { dimension: Dimension
    , flatten: Array CellState
    }

make: Dimension -> (Int -> CellState) -> Grid
make dim gen =
    Grid
        (dim)
        (Array.initialize
            ( dim.w * dim.h) gen)

getArea: Dimension -> Int
getArea dim =
    dim.w * dim.h

makeFromList: Dimension -> List CellState -> Maybe Grid
makeFromList dim list =
    if getArea dim /= List.length list then
        Nothing
    else
        Just <| Grid dim <| Array.fromList list

makeDimension: Int -> Int -> Dimension
makeDimension =
    Dimension

makePosition: Int -> Int -> Position
makePosition =
    Position

isWithinDimension: Dimension -> Position -> Bool
isWithinDimension d p =
    p.t >= 0
        && p.l >= 0
        && d.h > p.t
        && d.w > p.l

getStateAt: Grid -> Position -> CellState
getStateAt grid position =
    let
        maybePositon: Maybe CellState
        maybePositon =
            if isWithinDimension grid.dimension position then
                Array.get
                    (positionToFlat grid.dimension position)
                    (grid.flatten)
            else
                Nothing
    in
        case maybePositon of
            Nothing ->
                Empty
            Just state ->
                state

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

positionToFlat: Dimension -> Position -> Int
positionToFlat dim pos =
    pos.t * dim.h + pos.l
