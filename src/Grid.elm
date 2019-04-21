module Grid exposing
    ( FateOf
    , Grid
    , convertPositionFromFlat
    , convertPositionToFlat
    , generate
    , getNeighbourPositions
    , getStateAt
    , isWithinDimension
    , iterate
    , makeFromGridAndResize
    , makeFromList
    , run
    )

import Basic
import Dimension
import List.Extra
import Position


type alias Seeder state =
    Int -> state


type alias FateOf state =
    state
    -> List state
    -> state


type alias Grid state =
    { dimension : Dimension.Two
    , defaultState : state
    , fateOf : FateOf state
    , flatten : List state
    }


generate :
    Dimension.Two
    -> state
    -> FateOf state
    -> Seeder state
    -> Grid state
generate dim defaultState fateOf =
    List.Extra.initialize (dim.w * dim.h)
        >> Grid dim defaultState fateOf


makeFromList :
    Dimension.Two
    -> state
    -> FateOf state
    -> List state
    -> Maybe (Grid state)
makeFromList dim defaultState fateOf copied =
    if Dimension.getArea dim /= List.length copied then
        Nothing

    else
        Just <| Grid dim defaultState fateOf copied


makeFromGridAndChipCells :
    Grid state
    -> Dimension.Two
    -> state
    -> FateOf state
    -> (Int -> state -> ( Bool, state ))
    -> Maybe (Grid state)
makeFromGridAndChipCells grid dimension defaultState fateOf =
    Basic.flip List.indexedMap grid.flatten
        >> List.filter Tuple.first
        >> List.map Tuple.second
        >> Grid
            dimension
            defaultState
            fateOf
        >> Just


isInThisColumn :
    Dimension.Two
    -> Int
    -> Int
    -> Bool
isInThisColumn dim column =
    -- XXX something fishy, I ignore dimension and tell the column…
    modBy (column + 1) >> (/=) 0


isInThisRow :
    Dimension.Two
    -> Int
    -> Int
    -> Bool
isInThisRow dim row =
    modBy dim.w >> (/=) row


fetchStateOrGenerate :
    Grid state
    -> Seeder state
    -> List Position.Two
    -> List state
fetchStateOrGenerate grid seeder =
    List.map
        (\position ->
            if isWithinDimension grid.dimension position then
                getStateAt grid position

            else
                position
                    |> convertPositionToFlat grid.dimension
                    |> Maybe.withDefault 0
                    |> seeder
        )


makeFromGridAndResize :
    Grid state
    -> Dimension.Two
    -> Seeder state
    -> Grid state
makeFromGridAndResize grid newDimension seeder =
    let
        generateGrid : Dimension.Two -> List Position.Two
        generateGrid dim =
            List.range 0 (dim.h - 1)
                |> List.map (generateRow dim.w)
                |> List.concat
                |> List.map Position.fromTuple

        generateRow :
            Int
            -> Int
            -> List ( Int, Int ) -- row, col
        generateRow size line =
            List.map2
                Tuple.pair
                (List.repeat size line)
                (List.range 0 (size - 1))
    in
    generateGrid newDimension
        |> fetchStateOrGenerate grid seeder
        |> Grid newDimension grid.defaultState grid.fateOf



-- Manipulating positions within dimensions


isWithinDimension : Dimension.Two -> Position.Two -> Bool
isWithinDimension d p =
    p.t
        >= 0
        && p.l
        >= 0
        && d.h
        > p.t
        && d.w
        > p.l


isWithinFlattenDimension : Dimension.Two -> Int -> Bool
isWithinFlattenDimension d =
    (>) (Dimension.getArea d)



-- that's BS, should be able to make that with a turtle and distance mesurement


{-| Given a Dimension and a Position, returns a List of neighbouring positions within
the borders.
-}
getNeighbourPositions : Position.Two -> Dimension.Two -> List Position.Two
getNeighbourPositions p =
    let
        positions : List Position.Two
        positions =
            [ { p | t = p.t - 1, l = p.l - 1 }
            , { p | t = p.t - 1 }
            , { p | t = p.t - 1, l = p.l + 1 }
            , { p | t = p.t, l = p.l - 1 }
            , { p | t = p.t, l = p.l + 1 }
            , { p | t = p.t + 1, l = p.l - 1 }
            , { p | t = p.t + 1 }
            , { p | t = p.t + 1, l = p.l + 1 }
            ]
    in
    isWithinDimension >> Basic.flip List.filter positions


getNeighbourFlattenPositions : Dimension.Two -> Int -> List Int
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


convertPositionToFlat : Dimension.Two -> Position.Two -> Maybe Int
convertPositionToFlat dim pos =
    if pos.t >= dim.h || pos.l >= dim.w then
        Nothing

    else
        Just (pos.t * dim.w + pos.l)


convertPositionFromFlat : Dimension.Two -> Int -> Maybe Position.Two
convertPositionFromFlat dim flat =
    if isWithinFlattenDimension dim flat then
        Just
            (Position.Two
                (flat // dim.w)
                (flat - (flat // dim.w) * dim.w)
            )

    else
        Nothing



-- Inspect grid


getStateAtFlat : Grid state -> Int -> state
getStateAtFlat grid =
    Basic.flip
        List.Extra.getAt
        grid.flatten
        >> Maybe.withDefault grid.defaultState


getStateAt : Grid state -> Position.Two -> state
getStateAt grid position =
    let
        maybePosition : Maybe Position.Two
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
getNeighbourStatesFromFlattenPosition grid =
    getNeighbourFlattenPositions
        grid.dimension
        >> getStatesFromFlattenPositions
            grid


getNeighbourStatesFromPosition : Grid state -> Position.Two -> List state
getNeighbourStatesFromPosition grid =
    Basic.flip getNeighbourPositions grid.dimension
        >> getStatesFromPositions grid


getStatesFromPositions : Grid state -> List Position.Two -> List state
getStatesFromPositions grid =
    List.map (getStateAt grid)


getStatesFromFlattenPositions : Grid state -> List Int -> List state
getStatesFromFlattenPositions grid =
    List.map (getStateAtFlat grid)


fateOfState : Grid state -> Int -> state -> state
fateOfState grid pos =
    getNeighbourStatesFromFlattenPosition grid pos
        |> Basic.flip grid.fateOf



-- RUN FORREST! RUUUUUUN!


run : Grid state -> Grid state
run currentGrid =
    let
        fateAt : Int -> state -> state
        fateAt =
            fateOfState currentGrid
    in
    Grid
        currentGrid.dimension
        currentGrid.defaultState
        currentGrid.fateOf
        (List.indexedMap fateAt currentGrid.flatten)


packPositionState : Dimension.Two -> Int -> state -> ( Position.Two, state )
packPositionState dim flattenPosition =
    convertPositionFromFlat dim flattenPosition
        |> Maybe.withDefault (Position.Two 0 0)
        |> Tuple.pair


iterate : Grid state -> List ( Position.Two, state )
iterate grid =
    List.indexedMap
        (packPositionState grid.dimension)
        grid.flatten
