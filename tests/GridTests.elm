module GridTests exposing (suite)

import Grid

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
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
