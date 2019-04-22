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
    , iterateStateList
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
    , stateList : List state
    }


generate :
    Dimension.Two
    -> state
    -> FateOf state
    -> Seeder state
    -> Grid state
generate dim defaultState fateOf =
    List.Extra.initialize
        (dim.w * dim.h)
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
        copied
            |> iterateStateList dim
            |> List.map Tuple.second
            |> Grid dim defaultState fateOf
            |> Just


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
                seeder <| positionToFlat grid.dimension position
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



-- Conversion helpers


positionToFlat : Dimension.Two -> Position.Two -> Int
positionToFlat dim pos =
    pos.t * dim.w + pos.l


convertPositionToFlat : Dimension.Two -> Position.Two -> Maybe Int
convertPositionToFlat dim pos =
    if pos.t >= dim.h || pos.l >= dim.w then
        Nothing

    else
        positionToFlat dim pos
            |> Just


positionFromFlat : Dimension.Two -> Int -> Position.Two
positionFromFlat dim flat =
    Position.Two
        (flat // dim.w)
        (flat - (flat // dim.w) * dim.w)


convertPositionFromFlat : Dimension.Two -> Int -> Maybe Position.Two
convertPositionFromFlat dim flat =
    if isWithinFlattenDimension dim flat then
        positionFromFlat dim flat
            |> Just

    else
        Nothing



-- Inspect grid


getStateAtFlat : Grid state -> Int -> state
getStateAtFlat grid =
    Basic.flip
        List.Extra.getAt
        grid.stateList
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


getNeighbourStatesFromPosition : Grid state -> Position.Two -> List state
getNeighbourStatesFromPosition grid =
    Basic.flip getNeighbourPositions grid.dimension
        >> getStatesFromPositions grid


getStatesFromPositions : Grid state -> List Position.Two -> List state
getStatesFromPositions grid =
    List.map (getStateAt grid)


fateOfState : Grid state -> Position.Two -> state -> state
fateOfState grid pos =
    getNeighbourStatesFromPosition grid pos
        |> Basic.flip grid.fateOf



-- RUN FORREST! RUUUUUUN!


run : Grid state -> Grid state
run currentGrid =
    let
        fateAt : ( Position.Two, state ) -> state
        fateAt =
            Basic.uncurry (fateOfState currentGrid)
    in
    Grid
        currentGrid.dimension
        currentGrid.defaultState
        currentGrid.fateOf
        (iterate currentGrid |> List.map fateAt)


iterate : Grid state -> List ( Position.Two, state )
iterate grid =
    grid.stateList
        |> iterateStateList grid.dimension


iterateStateList : Dimension.Two -> List state -> List ( Position.Two, state )
iterateStateList dimension =
    List.indexedMap Tuple.pair
        >> List.map (Tuple.mapFirst (positionFromFlat dimension))
