module Grid exposing
    ( CellState(..)
    , Dimension
    , Grid

    , isWithinDimension
    , getStateAt
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
    Array CellState

make: Dimension -> (Int -> CellState) -> Grid
make dim =
    Array.initialize
        ( dim.w * dim.h)

getArea: Dimension -> Int
getArea dim =
    dim.w * dim.h

makeFromList: Dimension -> List CellState -> Maybe Grid
makeFromList dim list =
    if getArea dim /= List.length list then
        Nothing
    else
        Just <| Array.fromList list

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

getStateAt: Dimension -> Grid -> Position -> CellState
getStateAt dim grid position =
    let
        maybePositon: Maybe CellState
        maybePositon =
            if isWithinDimension dim position then
                Array.get
                    (position.t * dim.h + position.l)
                    (grid)
            else
                Nothing
    in
        case maybePositon of
            Nothing ->
                Empty
            Just state ->
                state
