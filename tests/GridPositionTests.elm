module GridPositionTests exposing
    ( coordinateFlatteningTests
    , coordinateWideningTests
    , simpleTest
    )

import Dimension
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Grid
import Grid.Position
import Test exposing (..)


simpleTest : Test
simpleTest =
    describe "Test coordinate system cherry picking"
        [ test "One cell grid"
            (\_ ->
                let
                    dim : Dimension.Two
                    dim =
                        Dimension.make 1 1
                in
                Grid.convertPositionToFlat dim
                    (Grid.Position.make 0 0)
                    |> Expect.equal (Just 0)
            )
        ]


coordinateFlatteningTests : Test
coordinateFlatteningTests =
    describe "Test coordinate system flattening"
        [ test "Test unicellular actual flatten position"
            (\_ ->
                let
                    dim : Dimension.Two
                    dim =
                        Dimension.make 1 1
                in
                Grid.convertPositionToFlat dim
                    (Grid.Position.make 0 0)
                    |> Expect.equal (Just 0)
            )
        , test "Test unicellular when requesting outrange coordinates"
            (\_ ->
                let
                    dim : Dimension.Two
                    dim =
                        Dimension.make 1 1
                in
                Grid.convertPositionToFlat dim
                    (Grid.Position.make 1 0)
                    |> Expect.equal Nothing
            )
        , test "Test multicellular inrange flatten position"
            (\_ ->
                let
                    dim : Dimension.Two
                    dim =
                        Dimension.make 3 3
                in
                Grid.convertPositionToFlat dim
                    (Grid.Position.make 2 2)
                    |> Expect.equal (Just 8)
            )
        , test "Test multicellular when requiring outrange coordinates"
            (\_ ->
                let
                    dim : Dimension.Two
                    dim =
                        Dimension.make 3 3
                in
                Grid.convertPositionToFlat dim
                    (Grid.Position.make 2 4)
                    |> Expect.equal Nothing
            )
        ]


coordinateWideningTests : Test
coordinateWideningTests =
    describe "Test coordinate system widening"
        [ test "Test unicellular actual position"
            (\_ ->
                let
                    dim : Dimension.Two
                    dim =
                        Dimension.make 1 1
                in
                Grid.convertPositionFromFlat dim 0
                    |> Expect.equal
                        (Just (Grid.Position.make 0 0))
            )
        , test "Test unicellular when requesting outrange coordinates"
            (\_ ->
                let
                    dim : Dimension.Two
                    dim =
                        Dimension.make 1 1
                in
                Grid.convertPositionFromFlat dim 10
                    |> Expect.equal Nothing
            )
        , test "Test multicellular inrange flatten position"
            (\_ ->
                let
                    dim : Dimension.Two
                    dim =
                        Dimension.make 3 3
                in
                Grid.convertPositionFromFlat dim 8
                    |> Expect.equal (Just (Grid.Position.make 2 2))
            )
        , test "Test multicellular when requiring outrange coordinates"
            (\_ ->
                let
                    dim : Dimension.Two
                    dim =
                        Dimension.make 3 3
                in
                Grid.convertPositionFromFlat dim 15
                    |> Expect.equal Nothing
            )
        , test "Test multicellular in grid (100 50) coordinates (5 5) from flatten (255)"
            (\_ ->
                let
                    dim : Dimension.Two
                    dim =
                        Dimension.make 100 50
                in
                Grid.convertPositionFromFlat dim 255
                    |> Expect.equal (Just (Grid.Position.make 5 5))
            )
        , test "Test multicellular in grid (100 50) coordinates (4 0) from flatten (200)"
            (\_ ->
                let
                    dim : Dimension.Two
                    dim =
                        Dimension.make 100 50
                in
                Grid.convertPositionFromFlat dim 200
                    |> Expect.equal (Just (Grid.Position.make 4 0))
            )
        , test "Test multicellular in grid (100 50) coordinates (3 49) from flatten (199)"
            (\_ ->
                let
                    dim : Dimension.Two
                    dim =
                        Dimension.make 100 50
                in
                Grid.convertPositionFromFlat dim 199
                    |> Expect.equal (Just (Grid.Position.make 3 49))
            )
        , test "Test multicellular in grid (100 50) coordinates (4 1) from flatten (201)"
            (\_ ->
                let
                    dim : Dimension.Two
                    dim =
                        Dimension.make 100 50
                in
                Grid.convertPositionFromFlat dim 201
                    |> Expect.equal (Just (Grid.Position.make 4 1))
            )
        , test "Test multicellular in grid (100 50) coordinates (3 48) from flatten (198)"
            (\_ ->
                let
                    dim : Dimension.Two
                    dim =
                        Dimension.make 100 50
                in
                Grid.convertPositionFromFlat dim 198
                    |> Expect.equal (Just (Grid.Position.make 3 48))
            )
        ]
