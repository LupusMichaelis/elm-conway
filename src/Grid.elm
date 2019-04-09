module Grid exposing
    ( Dimension
    , FateOf
    , Grid
    , Position
    , Seeder
    , convertPositionFromFlat
    , convertPositionToFlat
    , generate
    , getNeighbourPositions
    , getStateAt
    , isWithinDimension
    , iterate
    , makeDimension
    , makeFromGridAndResize
    , makeFromList
    , makePosition
    , run
    )

import Cell exposing (State(..))
import Grid.Dimension exposing (Dimension)
import Grid.Position exposing (Position)
import List.Extra


type alias Seeder state =
    Int -> state


type alias Dimension =
    Grid.Dimension.Dimension


type alias Position =
    Grid.Position.Position


type alias FateOf state =
    state
    -> List state
    -> state


type alias Grid state =
    { dimension : Dimension
    , defaultState : state
    , fateOf : FateOf state
    , flatten : List state
    }



-- Factories


makeDimension : Int -> Int -> Dimension
makeDimension =
    Grid.Dimension.make


makePosition : Int -> Int -> Position
makePosition =
    Grid.Position.make


generate :
    Dimension
    -> state
    -> FateOf state
    -> Seeder state
    -> Grid state
generate dim defaultState fateOf gen =
    Grid
        dim
        defaultState
        fateOf
        (List.Extra.initialize
            (dim.w * dim.h)
            gen
        )


makeFromList : Dimension -> state -> FateOf state -> List state -> Maybe (Grid state)
makeFromList dim defaultState fateOf copied =
    if Grid.Dimension.getArea dim /= List.length copied then
        Nothing

    else
        Just <| Grid dim defaultState fateOf copied


makeFromGridAndChipCells :
    Grid state
    -> Dimension
    -> state
    -> FateOf state
    -> (Int -> state -> ( Bool, state ))
    -> Maybe (Grid state)
makeFromGridAndChipCells grid dimension defaultState fateOf chipper =
    Just
        (Grid
            dimension
            defaultState
            fateOf
            (grid.flatten
                |> List.indexedMap chipper
                |> List.filter Tuple.first
                |> List.map Tuple.second
            )
        )


isInThisColumn : Dimension -> Int -> Int -> Bool
isInThisColumn dim column flatten =
    -- XXX something fishy, I ignore dimension and tell the column…
    flatten % (column + 1) /= 0


isInThisRow : Dimension -> Int -> Int -> Bool
isInThisRow dim row flatten =
    flatten % dim.w /= row


makeFromGridAndResize : Grid state -> Dimension -> Seeder state -> Grid state
makeFromGridAndResize grid newDimension seeder =
    let
        generateGrid : Dimension -> List ( Int, Int )
        generateGrid dim =
            List.range 0 (dim.h - 1)
                |> List.map (generateRow dim.w)
                |> List.concat

        generateRow :
            Int
            -> Int
            -> List ( Int, Int ) -- row, col
        generateRow size line =
            List.map2
                (,)
                (List.repeat size line)
                (List.range 0 (size - 1))

        fetchStateOrGenerate : Grid state -> Seeder state -> List Position -> List state
        fetchStateOrGenerate grid seeder positions =
            positions
                |> List.map
                    (\position ->
                        if isWithinDimension grid.dimension position then
                            getStateAt grid position

                        else
                            position
                                |> convertPositionToFlat grid.dimension
                                |> Maybe.withDefault 0
                                |> seeder
                    )
    in
    generateGrid newDimension
        |> List.map (\t -> Position (Tuple.first t) (Tuple.second t))
        |> fetchStateOrGenerate grid seeder
        |> Grid newDimension grid.defaultState grid.fateOf



-- Manipulating positions within dimensions


isWithinDimension : Dimension -> Position -> Bool
isWithinDimension d p =
    p.t
        >= 0
        && p.l
        >= 0
        && d.h
        > p.t
        && d.w
        > p.l


isWithinFlattenDimension : Dimension -> Int -> Bool
isWithinFlattenDimension d p =
    p < Grid.Dimension.getArea d



-- that's BS, should be able to make that with a turtle and distance mesurement


{-| Given a Dimension and a Position, returns a List of neighbouring positions within
the borders.
-}
getNeighbourPositions : Dimension -> Position -> List Position
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


getNeighbourFlattenPositions : Dimension -> Int -> List Int
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



-- Conversion helpers


convertPositionToFlat : Dimension -> Position -> Maybe Int
convertPositionToFlat dim pos =
    if pos.t >= dim.h || pos.l >= dim.w then
        Nothing

    else
        Just (pos.t * dim.w + pos.l)


convertPositionFromFlat : Dimension -> Int -> Maybe Position
convertPositionFromFlat dim flat =
    if isWithinFlattenDimension dim flat then
        Just
            (Position
                (flat // dim.w)
                (flat - (flat // dim.w) * dim.w)
            )

    else
        Nothing



-- Inspect grid


getStateAtFlat : Grid state -> Int -> state
getStateAtFlat grid flattenPosition =
    List.Extra.getAt
        flattenPosition
        grid.flatten
        |> Maybe.withDefault grid.defaultState


getStateAt : Grid state -> Position -> state
getStateAt grid position =
    let
        maybePosition : Maybe Position
        maybePosition =
            if isWithinDimension grid.dimension position then
                Just position

            else
                Nothing
    in
    convertPositionToFlat grid.dimension position
        |> Maybe.map (getStateAtFlat grid)
        |> Maybe.withDefault grid.defaultState


getNeighbourStatesFromFlattenPosition : Grid state -> Int -> List state
getNeighbourStatesFromFlattenPosition grid pos =
    let
        neighbours : List Int
        neighbours =
            getNeighbourFlattenPositions grid.dimension pos
    in
    getStatesFromFlattenPositions grid neighbours


getNeighbourStatesFromPosition : Grid state -> Position -> List state
getNeighbourStatesFromPosition grid pos =
    let
        neighbours : List Position
        neighbours =
            getNeighbourPositions grid.dimension pos
    in
    getStatesFromPositions grid neighbours


getStatesFromPositions : Grid state -> List Position -> List state
getStatesFromPositions grid positions =
    List.map (getStateAt grid) positions


getStatesFromFlattenPositions : Grid state -> List Int -> List state
getStatesFromFlattenPositions grid positions =
    List.map (getStateAtFlat grid) positions


fateOf : Grid state -> Int -> state -> state
fateOf grid pos currentCellState =
    let
        neighbourStates : List state
        neighbourStates =
            getNeighbourStatesFromFlattenPosition grid pos
    in
    grid.fateOf
        currentCellState
        neighbourStates



-- RUN FORREST! RUUUUUUN!


run : Grid state -> Grid state
run currentGrid =
    let
        fateAt : Int -> state -> state
        fateAt =
            fateOf currentGrid
    in
    Grid
        currentGrid.dimension
        currentGrid.defaultState
        currentGrid.fateOf
        (List.indexedMap fateAt currentGrid.flatten)


packPositionState : Dimension -> Int -> state -> ( Position, state )
packPositionState dim flattenPosition state =
    ( convertPositionFromFlat dim flattenPosition
        |> Maybe.withDefault (Position 0 0)
      -- XXX such ugliness
    , state
    )


iterate : Grid state -> List ( Position, state )
iterate grid =
    List.indexedMap
        (packPositionState grid.dimension)
        grid.flatten
