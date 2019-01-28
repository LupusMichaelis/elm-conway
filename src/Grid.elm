module Grid exposing
    ( CellState(..)
    , Dimension
    , Grid

    , isWithinDimension
    , getStateBetween
    , make
    , makeDimension
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

getStateBetween: Dimension -> Grid -> Position -> CellState
getStateBetween dim grid position =
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
