module GridTests exposing
    ( isWithinDimensionTests
    , getStateOfCellTests
    , testMakeGridFromStates
    )

import Grid

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

isWithinDimensionTests : Test
isWithinDimensionTests =
    describe "Given a grid dimensions, find out whether a point's within boundaries"
        [ test "Test (1,1) is inside boundaries (1,1)"
            (\_ ->
                Grid.isWithinDimension
                    (Grid.makeDimension 1 1)
                    (Grid.makePosition 1 1)

                |> Expect.false "expected to find with out boundaries"
            )

        , test "Test (0,0) is inside boundaries (1,1)"
            (\_ ->
                Grid.isWithinDimension
                    (Grid.makeDimension 1 1)
                    (Grid.makePosition 0 0)

                |> Expect.true "expected to find within boundaries"
            )

        , test "Test (0,1) is inside boundaries (1,1)"
            (\_ ->
                Grid.isWithinDimension
                    (Grid.makeDimension 1 1)
                    (Grid.makePosition 0 1)

                |> Expect.false "expected to find with out boundaries"
            )

        , test "Test (0,-1) is inside boundaries (1,1)"
            (\_ ->
                Grid.isWithinDimension
                    (Grid.makeDimension 1 1)
                    (Grid.makePosition 0 -1)

                |> Expect.false "expected to find with out boundaries"
            )

        , test "Test (-2,-1) is inside boundaries (1,1)"
            (\_ ->
                Grid.isWithinDimension
                    (Grid.makeDimension 1 1)
                    (Grid.makePosition -2 -1)

                |> Expect.false "expected to find with out boundaries"
            )
        ]

getStateOfCellTests : Test
getStateOfCellTests =
    describe "Given a grid, a position, find out cell's state"
        [ test "Test the unique cell is empty"
            (\_ ->
                let
                    dim =
                        Grid.makeDimension 1 1
                in
                    Grid.getStateAt
                        (dim)
                        (Grid.make dim (\_ -> Grid.Empty))
                        (Grid.makePosition 0 0)
                    |> Expect.equal Grid.Empty
            )
        , test "Test the unique cell is alive!"
            (\_ ->
                let
                    dim =
                        Grid.makeDimension 1 1
                in
                    Grid.getStateAt
                        (dim)
                        (Grid.make dim (\_ -> Grid.Live))
                        (Grid.makePosition 0 0)
                    |> Expect.equal Grid.Live
            )
        , test "Test the outer cell is empty!"
            (\_ ->
                let
                    dim =
                        Grid.makeDimension 1 1
                in
                    Grid.getStateAt
                        (dim)
                        (Grid.make dim (\_ -> Grid.Live))
                        (Grid.makePosition 10 10)
                    |> Expect.equal Grid.Empty
            )
        ]

testMakeGridFromStates : Test
testMakeGridFromStates =
    describe "Test grid making from a list of states"
        [ test "Test same size"
            (\_ ->
                let
                    dim: Grid.Dimension
                    dim =
                        Grid.makeDimension 1 1

                    list: List Grid.CellState
                    list =
                        [ Grid.Empty ]
                in
                    Grid.makeFromList dim list
                        |> Expect.notEqual Nothing
            )
        , test "Test different size"
            (\_ ->
                let
                    dim: Grid.Dimension
                    dim =
                        Grid.makeDimension 1 2

                    list: List Grid.CellState
                    list =
                        [ Grid.Empty ]
                in
                    Grid.makeFromList dim list
                        |> Expect.equal Nothing
            )
        ]
