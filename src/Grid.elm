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
    , iterate
    , getStateAt
    , getNeighbourPositions
    , generate
    , makeDimension
    , makeFromList
    , makePosition
    , run
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

generate: Dimension -> (Int -> CellState) -> Grid
generate dim gen =
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

isWithinFlattenDimension: Dimension -> Int -> Bool
isWithinFlattenDimension d p =
    p < Grid.Dimension.getArea d

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

getNeighbourFlattenPositions: Dimension -> Int -> Array Int
getNeighbourFlattenPositions dim p =
    [ p - dim.w - 1
    , p - dim.w
    , p - dim.w + 1
    , p - 1
    , p + 1
    , p + dim.w - 1
    , p + dim.w
    , p + dim.w + 1
    ]
        |> List.filter (isWithinFlattenDimension dim)
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
    if isWithinFlattenDimension dim flat then
        Just (Position
                (flat // dim.h)
                (flat % dim.h))
    else
        Nothing


-- Inspect grid

getStateAtFlat: Grid -> Int -> CellState
getStateAtFlat grid flattenPosition =
    Array.get
        flattenPosition
        grid.flatten
    |> Maybe.withDefault Empty

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
            |> Maybe.map (getStateAtFlat grid)
            |> Maybe.withDefault Empty

getNeighbourStatesFromFlattenPosition: Grid -> Int -> Array CellState
getNeighbourStatesFromFlattenPosition grid pos =
    let
        neighbours: Array Int
        neighbours =
            getNeighbourFlattenPositions grid.dimension pos

    in
        getStatesFromFlattenPositions grid neighbours

getNeighbourStatesFromPosition: Grid -> Position -> Array CellState
getNeighbourStatesFromPosition grid pos =
    let
        neighbours: Array Position
        neighbours =
            getNeighbourPositions grid.dimension pos

    in
        getStatesFromPositions grid neighbours

getStatesFromPositions: Grid -> Array Position -> Array CellState
getStatesFromPositions grid positions =
    Array.map (getStateAt grid) positions

getStatesFromFlattenPositions: Grid -> Array Int -> Array CellState
getStatesFromFlattenPositions grid positions =
    Array.map (getStateAtFlat grid) positions

fateOf: Grid -> Int -> CellState -> CellState
fateOf grid pos currentCellState =
    let
        neighbourStates: Array CellState
        neighbourStates =
            getNeighbourStatesFromFlattenPosition grid pos
    in
        Grid.Cell.fateOf currentCellState neighbourStates


-- RUN FORREST! RUUUUUUN!

run: Grid -> Grid
run currentGrid =
    let
        fateAt: Int -> CellState -> CellState
        fateAt =
            fateOf currentGrid
    in
        Grid
            currentGrid.dimension
            (Array.indexedMap fateAt currentGrid.flatten)

packPositionState: Dimension -> Int -> CellState -> (Position, CellState)
packPositionState dim flattenPosition state =
    ( convertPositionFromFlat dim flattenPosition
        |> Maybe.withDefault (Position 0 0)         -- XXX such ugliness
    , state)

iterate: Grid -> Array (Position, CellState)
iterate grid =
    Array.indexedMap
        (packPositionState grid.dimension)
        (grid.flatten)
