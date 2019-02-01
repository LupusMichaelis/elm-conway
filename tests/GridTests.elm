module GridTests exposing
    ( isWithinDimensionTests
    , getStateOfCellTests
    , makeGridFromStatesTests
    , neighbourhoodTests
    , resizeTests
    )

import Grid
import Grid.Cell

import Array

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

isWithinDimensionTests: Test
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

getStateOfCellTests: Test
getStateOfCellTests =
    describe "Given a grid, a position, find out cell's state"
        [ test "Test the unique cell is empty"
            (\_ ->
                let
                    dim =
                        Grid.makeDimension 1 1
                in
                    Grid.getStateAt
                        (Grid.generate dim (\_ -> Grid.Cell.Deceased))
                        (Grid.makePosition 0 0)
                    |> Expect.equal Grid.Cell.Deceased
            )
        , test "Test the unique cell is alive!"
            (\_ ->
                let
                    dim =
                        Grid.makeDimension 1 1
                in
                    Grid.getStateAt
                        (Grid.generate dim (\_ -> Grid.Cell.Live))
                        (Grid.makePosition 0 0)
                    |> Expect.equal Grid.Cell.Live
            )
        , test "Test the outer cell is empty!"
            (\_ ->
                let
                    dim =
                        Grid.makeDimension 1 1
                in
                    Grid.getStateAt
                        (Grid.generate dim (\_ -> Grid.Cell.Live))
                        (Grid.makePosition 10 10)
                    |> Expect.equal Grid.Cell.Deceased
            )
        ]

makeGridFromStatesTests: Test
makeGridFromStatesTests =
    describe "Test grid making from a list of states"
        [ test "Test same size"
            (\_ ->
                let
                    dim: Grid.Dimension
                    dim =
                        Grid.makeDimension 1 1

                    list: List Grid.CellState
                    list =
                        [ Grid.Cell.Deceased ]
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
                        [ Grid.Cell.Deceased ]
                in
                    Grid.makeFromList dim list
                        |> Expect.equal Nothing
            )
        ]

neighbourhoodTests: Test
neighbourhoodTests =
    describe "Test neighbourhood"
        [ test "Test unicellular"
            (\_ ->
                let
                    grid: Grid.Grid
                    grid =
                        Grid.generate
                            (Grid.makeDimension 1 1)
                            (\_ -> Grid.Cell.Deceased)

                    neighbours: Array.Array Grid.Position
                    neighbours =
                        Grid.getNeighbourPositions
                            (grid.dimension)
                            (Grid.makePosition 0 0)
                in
                    neighbours
                        |> Expect.equal (Array.fromList [])
            )
        , test "Test multicellular at origin"
            (\_ ->
                let
                    grid: Grid.Grid
                    grid =
                        Grid.generate
                            (Grid.makeDimension 10 10)
                            (\_ -> Grid.Cell.Deceased)

                    neighbours: Array.Array Grid.Position
                    neighbours =
                        Grid.getNeighbourPositions
                            (grid.dimension)
                            (Grid.makePosition 0 0)

                    expected: Array.Array Grid.Position
                    expected =
                        []  -- order matters
                            |> (::) (Grid.makePosition 1 1) -- third
                            |> (::) (Grid.makePosition 1 0) -- second
                            |> (::) (Grid.makePosition 0 1) -- first
                            |> Array.fromList
                in
                    neighbours
                        |> Expect.equal expected
            )
        , test "Test multicellular in middle-ish"
            (\_ ->
                let
                    grid: Grid.Grid
                    grid =
                        Grid.generate
                            (Grid.makeDimension 10 10)
                            (\_ -> Grid.Cell.Deceased)

                    neighbours: Array.Array Grid.Position
                    neighbours =
                        Grid.getNeighbourPositions
                            (grid.dimension)
                            (Grid.makePosition 5 5)

                    expected: Array.Array Grid.Position
                    expected =
                        []  -- order matters
                            |> (::) (Grid.makePosition 6 6)
                            |> (::) (Grid.makePosition 6 5)
                            |> (::) (Grid.makePosition 6 4)
                            |> (::) (Grid.makePosition 5 6)
                            |> (::) (Grid.makePosition 5 4)
                            |> (::) (Grid.makePosition 4 6)
                            |> (::) (Grid.makePosition 4 5)
                            |> (::) (Grid.makePosition 4 4)
                            |> Array.fromList
                in
                    neighbours
                        |> Expect.equal expected
            )
        , test "Test multicellular at end of space"
            (\_ ->
                let
                    grid: Grid.Grid
                    grid =
                        Grid.generate
                            (Grid.makeDimension 10 10)
                            (\_ -> Grid.Cell.Deceased)

                    neighbours: Array.Array Grid.Position
                    neighbours =
                        Grid.getNeighbourPositions
                            (grid.dimension)
                            (Grid.makePosition
                                (grid.dimension.w - 1)
                                (grid.dimension.h - 1)
                            )

                    expected: Array.Array Grid.Position
                    expected =
                        []  -- order matters
                            |> (::) (Grid.makePosition 9 8) -- third
                            |> (::) (Grid.makePosition 8 9) -- second
                            |> (::) (Grid.makePosition 8 8) -- first
                            |> Array.fromList
                in
                    neighbours
                        |> Expect.equal expected
            )
        ]

resizeTests: Test
resizeTests =
    let
        l = Grid.Cell.Live
        d = Grid.Cell.Deceased
    in
        describe "Test resizing grid"
            [ test "Test resizing empty grid's width"
                (\_ ->
                    let
                        original: Maybe Grid.Grid
                        original =
                            [ d, d
                            , d, d
                            ]
                            |> Grid.makeFromList
                                (Grid.makeDimension 2 2)

                        enlargedSize: Grid.Dimension
                        enlargedSize =
                            Grid.makeDimension 2 3

                        enlarged: Maybe Grid.Grid
                        enlarged =
                            [ d, d, d
                            , d, d, d
                            ]
                            |> Grid.makeFromList
                                enlargedSize
                    in
                        original
                            |> Maybe.map
                                (\g ->
                                     Grid.makeFromGridAndResize
                                         g
                                         enlargedSize
                                         (\_ -> Grid.Cell.Deceased)
                                )
                            |> Expect.equal enlarged
                )
            , test "Test resizing empty grid's height"
                (\_ ->
                    let
                        original: Maybe Grid.Grid
                        original =
                            [ d, d
                            , d, d
                            ]
                            |> Grid.makeFromList
                                (Grid.makeDimension 2 2)

                        enlargedSize: Grid.Dimension
                        enlargedSize =
                            Grid.makeDimension 3 2

                        enlarged: Maybe Grid.Grid
                        enlarged =
                            [ d, d
                            , d, d
                            , d, d
                            ]
                            |> Grid.makeFromList
                                enlargedSize
                    in
                        original
                            |> Maybe.map
                                (\g ->
                                     Grid.makeFromGridAndResize
                                         g
                                         enlargedSize
                                         (\_ -> Grid.Cell.Deceased)
                                )
                            |> Expect.equal enlarged
                )
            , test "Test resizing inhabited grid's width"
                (\_ ->
                    let
                        original: Maybe Grid.Grid
                        original =
                            [ d, d
                            , d, l
                            ]
                            |> Grid.makeFromList
                                (Grid.makeDimension 2 2)

                        enlargedSize: Grid.Dimension
                        enlargedSize =
                            Grid.makeDimension 2 3

                        enlarged: Maybe Grid.Grid
                        enlarged =
                            [ d, d, d
                            , d, l, d
                            ]
                            |> Grid.makeFromList
                                enlargedSize
                    in
                        original
                            |> Maybe.map
                                (\g ->
                                     Grid.makeFromGridAndResize
                                         g
                                         enlargedSize
                                         (\_ -> Grid.Cell.Deceased)
                                )
                            |> Expect.equal enlarged
                )
            , test "Test resizing inhabited grid's height"
                (\_ ->
                    let
                        original: Maybe Grid.Grid
                        original =
                            [ d, d
                            , d, l
                            ]
                            |> Grid.makeFromList
                                (Grid.makeDimension 2 2)

                        enlargedSize: Grid.Dimension
                        enlargedSize =
                            Grid.makeDimension 3 2

                        enlarged: Maybe Grid.Grid
                        enlarged =
                            [ d, d
                            , d, l
                            , d, d
                            ]
                            |> Grid.makeFromList
                                enlargedSize
                    in
                        original
                            |> Maybe.map
                                (\g ->
                                     Grid.makeFromGridAndResize
                                         g
                                         enlargedSize
                                         (\_ -> Grid.Cell.Deceased)
                                )
                            |> Expect.equal enlarged
                )
            , test "Test resizing inhabited grid's width with dead people"
                (\_ ->
                    let
                        original: Maybe Grid.Grid
                        original =
                            [ d, d
                            , d, l
                            ]
                            |> Grid.makeFromList
                                (Grid.makeDimension 2 2)

                        enlargedSize: Grid.Dimension
                        enlargedSize =
                            Grid.makeDimension 2 3

                        enlarged: Maybe Grid.Grid
                        enlarged =
                            [ d, d, d
                            , d, l, d
                            ]
                            |> Grid.makeFromList
                                enlargedSize
                    in
                        original
                            |> Maybe.map
                                (\g ->
                                     Grid.makeFromGridAndResize
                                         g
                                         enlargedSize
                                         (\_ -> Grid.Cell.Deceased)
                                )
                            |> Expect.equal enlarged
                )
            , test "Test resizing inhabited grid's height with dead people"
                (\_ ->
                    let
                        original: Maybe Grid.Grid
                        original =
                            [ d, d
                            , d, l
                            ]
                            |> Grid.makeFromList
                                (Grid.makeDimension 2 2)

                        enlargedSize: Grid.Dimension
                        enlargedSize =
                            Grid.makeDimension 3 2

                        enlarged: Maybe Grid.Grid
                        enlarged =
                            [ d, d
                            , d, l
                            , d, d
                            ]
                            |> Grid.makeFromList
                                enlargedSize
                    in
                        original
                            |> Maybe.map
                                (\g ->
                                     Grid.makeFromGridAndResize
                                         g
                                         enlargedSize
                                         (\_ -> Grid.Cell.Deceased)
                                )
                            |> Expect.equal enlarged
                )
            ]
