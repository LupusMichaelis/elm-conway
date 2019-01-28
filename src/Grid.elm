module Grid exposing
    ( CellState(..)
    , Dimension
    , Grid
    , makeGrid
    , makeGridDimension
    )

type CellState =
    Live            -- happy cell shanting around
    | Deceased      -- a corpse's lying there
    | Empty         -- no live cell's present

type alias Dimension =
    { w: Int    -- width
    , h: Int    -- height
    }

type alias Grid =
    List CellState

makeGridDimension: Int -> Int -> Dimension
makeGridDimension width height =
    Dimension width height

makeGrid: Dimension -> CellState -> Grid
makeGrid d e =
     List.repeat (d.w * d.h) e
