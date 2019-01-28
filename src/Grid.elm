module Grid exposing
    ( CellState(..)
    , Dimension
    , Grid
    , make
    , makeDimension
    )

import Array exposing (Array)

type CellState =
    Live            -- happy cell shanting around
    | Deceased      -- a corpse's lying there
    | Empty         -- no live cell's present

type alias Dimension =
    { w: Int    -- width
    , h: Int    -- height
    }

type alias Grid =
    Array CellState

makeDimension: Int -> Int -> Dimension
makeDimension width height =
    Dimension width height

make: Dimension -> CellState -> Grid
make d e =
     Array.repeat (d.w * d.h) e
