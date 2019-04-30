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
    , makeFromArray
    , makeFromGridAndResize
    , makeFromList
    , run
    )

import Array exposing (Array)
import Array.Extra
import Basic
import Dimension
import Position
import Seeder


type alias FateOf state =
    state
    -> Array state
    -> state


type alias Grid state =
    { dimension : Dimension.Two
    , defaultState : state
    , fateOf : FateOf state
    , stateList : Array state
    }


generate :
    Dimension.Two
    -> state
    -> FateOf state
    -> Seeder.Type state
    -> Grid state
generate dim defaultState fateOf seeder =
    let
        wrappedGenerator : Int -> state
        wrappedGenerator =
            \position ->
                case seeder of
                    Seeder.Value value ->
                        value

                    Seeder.Index generator ->
                        generator position

                    Seeder.Position generator ->
                        generator <| positionFromFlat dim position

                    Seeder.Dimension generator ->
                        generator dim <| positionFromFlat dim position
    in
    Array.initialize
        (dim.w * dim.h)
        wrappedGenerator
        |> Grid dim defaultState fateOf


makeFromList :
    Dimension.Two
    -> state
    -> FateOf state
    -> List state
    -> Maybe (Grid state)
makeFromList dim defaultState fateOf copied =
    Array.fromList copied
        |> makeFromArray dim defaultState fateOf


makeFromArray :
    Dimension.Two
    -> state
    -> FateOf state
    -> Array state
    -> Maybe (Grid state)
makeFromArray dim defaultState fateOf copied =
    if Dimension.getArea dim /= Array.length copied then
        Nothing

    else
        copied
            |> iterateStateList dim
            |> Array.map Tuple.second
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
    -> Seeder.Type state
    -> Array Position.Two
    -> Array state
fetchStateOrGenerate grid seeder =
    Array.map
        (\position ->
            if isWithinDimension grid.dimension position then
                getStateAt grid position

            else
                case seeder of
                    Seeder.Value value ->
                        value

                    Seeder.Index generator ->
                        generator <| positionToFlat grid.dimension position

                    Seeder.Position generator ->
                        generator position

                    Seeder.Dimension generator ->
                        generator grid.dimension position
        )


makeFromGridAndResize :
    Grid state
    -> Dimension.Two
    -> Seeder.Type state
    -> Grid state
makeFromGridAndResize grid newDimension seeder =
    let
        generateGrid : Dimension.Two -> Array Position.Two
        generateGrid dim =
            (List.range 0 (dim.h - 1) |> Array.fromList)
                |> Array.map (generateRow dim.w)
                |> Array.foldr Array.append Array.empty
                |> Array.map Position.fromTuple

        generateRow :
            Int
            -> Int
            -> Array ( Int, Int ) -- row, col
        generateRow size line =
            Array.Extra.map2
                Tuple.pair
                (Array.repeat size line)
                (List.range 0 (size - 1) |> Array.fromList)
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


{-| Given a Dimension and a Position, returns a Array of neighbouring positions within
the borders.
-}
getNeighbourPositions : Position.Two -> Dimension.Two -> Array Position.Two
getNeighbourPositions p =
    let
        positions : Array Position.Two
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
                |> Array.fromList
    in
    isWithinDimension >> Basic.flip Array.filter positions



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
        Array.get
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


getNeighbourStatesFromPosition : Grid state -> Position.Two -> Array state
getNeighbourStatesFromPosition grid =
    Basic.flip getNeighbourPositions grid.dimension
        >> getStatesFromPositions grid


getStatesFromPositions : Grid state -> Array Position.Two -> Array state
getStatesFromPositions grid =
    Array.map (getStateAt grid)


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
        (iterate currentGrid |> Array.map fateAt)


iterate : Grid state -> Array ( Position.Two, state )
iterate grid =
    grid.stateList
        |> iterateStateList grid.dimension


iterateStateList : Dimension.Two -> Array state -> Array ( Position.Two, state )
iterateStateList dimension =
    Array.indexedMap Tuple.pair
        >> Array.map (Tuple.mapFirst (positionFromFlat dimension))
