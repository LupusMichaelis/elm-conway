module GridTests exposing
    ( getStateOfCellTests
    , isWithinDimensionTests
    , makeGridFromStatesTests
    , neighbourhoodTests
    , resizeTests
    )

import Cell
import Dimension
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Grid
import Position
import Test exposing (..)


isWithinDimensionTests : Test
isWithinDimensionTests =
    describe "Given a grid dimensions, find out whether a point's within boundaries"
        [ test "Test (1,1) is inside boundaries (1,1)"
            (\_ ->
                Grid.isWithinDimension
                    (Dimension.make 1 1)
                    (Position.make 1 1)
                    |> Expect.false "expected to find with out boundaries"
            )
        , test "Test (0,0) is inside boundaries (1,1)"
            (\_ ->
                Grid.isWithinDimension
                    (Dimension.make 1 1)
                    (Position.make 0 0)
                    |> Expect.true "expected to find within boundaries"
            )
        , test "Test (0,1) is inside boundaries (1,1)"
            (\_ ->
                Grid.isWithinDimension
                    (Dimension.make 1 1)
                    (Position.make 0 1)
                    |> Expect.false "expected to find with out boundaries"
            )
        , test "Test (0,-1) is inside boundaries (1,1)"
            (\_ ->
                Grid.isWithinDimension
                    (Dimension.make 1 1)
                    (Position.make 0 -1)
                    |> Expect.false "expected to find with out boundaries"
            )
        , test "Test (-2,-1) is inside boundaries (1,1)"
            (\_ ->
                Grid.isWithinDimension
                    (Dimension.make 1 1)
                    (Position.make -2 -1)
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
                        Dimension.make 1 1
                in
                Grid.getStateAt
                    (Grid.generate dim
                        Cell.Deceased
                        Cell.fateOf
                        (\_ -> Cell.Deceased)
                    )
                    (Position.make 0 0)
                    |> Expect.equal Cell.Deceased
            )
        , test "Test the unique cell is alive!"
            (\_ ->
                let
                    dim =
                        Dimension.make 1 1
                in
                Grid.getStateAt
                    (Grid.generate dim
                        Cell.Deceased
                        Cell.fateOf
                        (\_ -> Cell.Live)
                    )
                    (Position.make 0 0)
                    |> Expect.equal Cell.Live
            )
        , test "Test the outer cell is empty!"
            (\_ ->
                let
                    dim =
                        Dimension.make 1 1
                in
                Grid.getStateAt
                    (Grid.generate dim
                        Cell.Deceased
                        Cell.fateOf
                        (\_ -> Cell.Live)
                    )
                    (Position.make 10 10)
                    |> Expect.equal Cell.Deceased
            )
        ]


makeGridFromStatesTests : Test
makeGridFromStatesTests =
    describe "Test grid making from a list of states"
        [ test "Test same size"
            (\_ ->
                let
                    dim : Dimension.Two
                    dim =
                        Dimension.make 1 1

                    list : List Cell.State
                    list =
                        [ Cell.Deceased ]
                in
                Grid.makeFromList
                    dim
                    Cell.Deceased
                    Cell.fateOf
                    list
                    |> Expect.notEqual Nothing
            )
        , test "Test different size"
            (\_ ->
                let
                    dim : Dimension.Two
                    dim =
                        Dimension.make 1 2

                    list : List Cell.State
                    list =
                        [ Cell.Deceased ]
                in
                Grid.makeFromList
                    dim
                    Cell.Deceased
                    Cell.fateOf
                    list
                    |> Expect.equal Nothing
            )
        ]


neighbourhoodTests : Test
neighbourhoodTests =
    describe "Test neighbourhood"
        [ test "Test unicellular"
            (\_ ->
                let
                    grid : Grid.Grid Cell.State
                    grid =
                        Grid.generate
                            (Dimension.make 1 1)
                            Cell.Deceased
                            Cell.fateOf
                            (\_ -> Cell.Deceased)

                    neighbours : List Position.Two
                    neighbours =
                        Grid.getNeighbourPositions
                            grid.dimension
                            (Position.make 0 0)
                in
                neighbours
                    |> Expect.equal []
            )
        , test "Test multicellular at origin"
            (\_ ->
                let
                    grid : Grid.Grid Cell.State
                    grid =
                        Grid.generate
                            (Dimension.make 10 10)
                            Cell.Deceased
                            Cell.fateOf
                            (\_ -> Cell.Deceased)

                    neighbours : List Position.Two
                    neighbours =
                        Grid.getNeighbourPositions
                            grid.dimension
                            (Position.make 0 0)

                    expected : List Position.Two
                    expected =
                        []
                            -- order matters
                            |> {- third -} (::) (Position.make 1 1)
                            |> {- second -} (::) (Position.make 1 0)
                            |> {- first -} (::) (Position.make 0 1)
                in
                neighbours
                    |> Expect.equal expected
            )
        , test "Test multicellular in middle-ish"
            (\_ ->
                let
                    grid : Grid.Grid Cell.State
                    grid =
                        Grid.generate
                            (Dimension.make 10 10)
                            Cell.Deceased
                            Cell.fateOf
                            (\_ -> Cell.Deceased)

                    neighbours : List Position.Two
                    neighbours =
                        Grid.getNeighbourPositions
                            grid.dimension
                            (Position.make 5 5)

                    expected : List Position.Two
                    expected =
                        []
                            -- order matters
                            |> (::) (Position.make 6 6)
                            |> (::) (Position.make 6 5)
                            |> (::) (Position.make 6 4)
                            |> (::) (Position.make 5 6)
                            |> (::) (Position.make 5 4)
                            |> (::) (Position.make 4 6)
                            |> (::) (Position.make 4 5)
                            |> (::) (Position.make 4 4)
                in
                neighbours
                    |> Expect.equal expected
            )
        , test "Test multicellular at end of space"
            (\_ ->
                let
                    grid : Grid.Grid Cell.State
                    grid =
                        Grid.generate
                            (Dimension.make 10 10)
                            Cell.Deceased
                            Cell.fateOf
                            (\_ -> Cell.Deceased)

                    neighbours : List Position.Two
                    neighbours =
                        Grid.getNeighbourPositions
                            grid.dimension
                            (Position.make
                                (grid.dimension.w - 1)
                                (grid.dimension.h - 1)
                            )

                    expected : List Position.Two
                    expected =
                        []
                            -- order matters
                            |> {- third -} (::) (Position.make 9 8)
                            |> {- second -} (::) (Position.make 8 9)
                            |> {- first -} (::) (Position.make 8 8)
                in
                neighbours
                    |> Expect.equal expected
            )
        ]


resizeTests : Test
resizeTests =
    let
        l =
            Cell.Live

        d =
            Cell.Deceased
    in
    describe "Test resizing grid"
        [ test "Test resizing empty grid's width"
            (\_ ->
                let
                    original : Maybe (Grid.Grid Cell.State)
                    original =
                        [ d
                        , d
                        , d
                        , d
                        ]
                            |> Grid.makeFromList
                                (Dimension.make 2 2)
                                Cell.Deceased
                                Cell.fateOf

                    enlargedSize : Dimension.Two
                    enlargedSize =
                        Dimension.make 2 3

                    enlarged : Maybe (Grid.Grid Cell.State)
                    enlarged =
                        [ d
                        , d
                        , d
                        , d
                        , d
                        , d
                        ]
                            |> Grid.makeFromList
                                enlargedSize
                                Cell.Deceased
                                Cell.fateOf
                in
                original
                    |> Maybe.map
                        (\g ->
                            Grid.makeFromGridAndResize
                                g
                                enlargedSize
                                (\_ -> Cell.Deceased)
                        )
                    |> Expect.equal enlarged
            )
        , test "Test resizing empty grid's height"
            (\_ ->
                let
                    original : Maybe (Grid.Grid Cell.State)
                    original =
                        [ d
                        , d
                        , d
                        , d
                        ]
                            |> Grid.makeFromList
                                (Dimension.make 2 2)
                                Cell.Deceased
                                Cell.fateOf

                    enlargedSize : Dimension.Two
                    enlargedSize =
                        Dimension.make 3 2

                    enlarged : Maybe (Grid.Grid Cell.State)
                    enlarged =
                        [ d
                        , d
                        , d
                        , d
                        , d
                        , d
                        ]
                            |> Grid.makeFromList
                                enlargedSize
                                Cell.Deceased
                                Cell.fateOf
                in
                original
                    |> Maybe.map
                        (\g ->
                            Grid.makeFromGridAndResize
                                g
                                enlargedSize
                                (\_ -> Cell.Deceased)
                        )
                    |> Expect.equal enlarged
            )
        , test "Test resizing inhabited grid's width"
            (\_ ->
                let
                    original : Maybe (Grid.Grid Cell.State)
                    original =
                        [ d
                        , d
                        , d
                        , l
                        ]
                            |> Grid.makeFromList
                                (Dimension.make 2 2)
                                Cell.Deceased
                                Cell.fateOf

                    enlargedSize : Dimension.Two
                    enlargedSize =
                        Dimension.make 2 3

                    enlarged : Maybe (Grid.Grid Cell.State)
                    enlarged =
                        [ d
                        , d
                        , d
                        , d
                        , l
                        , d
                        ]
                            |> Grid.makeFromList
                                enlargedSize
                                Cell.Deceased
                                Cell.fateOf
                in
                original
                    |> Maybe.map
                        (\g ->
                            Grid.makeFromGridAndResize
                                g
                                enlargedSize
                                (\_ -> Cell.Deceased)
                        )
                    |> Expect.equal enlarged
            )
        , test "Test resizing inhabited grid's height"
            (\_ ->
                let
                    original : Maybe (Grid.Grid Cell.State)
                    original =
                        [ d
                        , d
                        , d
                        , l
                        ]
                            |> Grid.makeFromList
                                (Dimension.make 2 2)
                                Cell.Deceased
                                Cell.fateOf

                    enlargedSize : Dimension.Two
                    enlargedSize =
                        Dimension.make 3 2

                    enlarged : Maybe (Grid.Grid Cell.State)
                    enlarged =
                        [ d
                        , d
                        , d
                        , l
                        , d
                        , d
                        ]
                            |> Grid.makeFromList
                                enlargedSize
                                Cell.Deceased
                                Cell.fateOf
                in
                original
                    |> Maybe.map
                        (\g ->
                            Grid.makeFromGridAndResize
                                g
                                enlargedSize
                                (\_ -> Cell.Deceased)
                        )
                    |> Expect.equal enlarged
            )
        , test "Test resizing inhabited grid's width with dead people"
            (\_ ->
                let
                    original : Maybe (Grid.Grid Cell.State)
                    original =
                        [ d
                        , d
                        , d
                        , l
                        ]
                            |> Grid.makeFromList
                                (Dimension.make 2 2)
                                Cell.Deceased
                                Cell.fateOf

                    enlargedSize : Dimension.Two
                    enlargedSize =
                        Dimension.make 2 3

                    enlarged : Maybe (Grid.Grid Cell.State)
                    enlarged =
                        [ d
                        , d
                        , d
                        , d
                        , l
                        , d
                        ]
                            |> Grid.makeFromList
                                enlargedSize
                                Cell.Deceased
                                Cell.fateOf
                in
                original
                    |> Maybe.map
                        (\g ->
                            Grid.makeFromGridAndResize
                                g
                                enlargedSize
                                (\_ -> Cell.Deceased)
                        )
                    |> Expect.equal enlarged
            )
        , test "Test resizing inhabited grid's height with dead people"
            (\_ ->
                let
                    original : Maybe (Grid.Grid Cell.State)
                    original =
                        [ d
                        , d
                        , d
                        , l
                        ]
                            |> Grid.makeFromList
                                (Dimension.make 2 2)
                                Cell.Deceased
                                Cell.fateOf

                    enlargedSize : Dimension.Two
                    enlargedSize =
                        Dimension.make 3 2

                    enlarged : Maybe (Grid.Grid Cell.State)
                    enlarged =
                        [ d
                        , d
                        , d
                        , l
                        , d
                        , d
                        ]
                            |> Grid.makeFromList
                                enlargedSize
                                Cell.Deceased
                                Cell.fateOf
                in
                original
                    |> Maybe.map
                        (\g ->
                            Grid.makeFromGridAndResize
                                g
                                enlargedSize
                                (\_ -> Cell.Deceased)
                        )
                    |> Expect.equal enlarged
            )
        ]
