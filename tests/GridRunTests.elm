module GridRunTests exposing
    ( basicArrangement
    , cellForetoldFateTests
    , cellInRectangularSandbox
    )

import Array
import Cell
import Dimension
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Grid
import Test exposing (..)


cellForetoldFateTests : Test
cellForetoldFateTests =
    let
        l =
            Cell.Live

        d =
            Cell.Deceased
    in
    describe
        """
            Prophecies shall be fulfilled
            (https://en.wikipedia.org/wiki/Conway's_Game_of_Life#Examples_of_patterns)
            """
        [ test "Still life: block"
            (\_ ->
                let
                    block : List Cell.State
                    block =
                        [ [ d, d, d, d ]
                        , [ d, l, l, d ]
                        , [ d, l, l, d ]
                        , [ d, d, d, d ]
                        ]
                            |> List.concat

                    grid : Maybe (Grid.Grid Cell.State)
                    grid =
                        Grid.makeFromList
                            (Dimension.make 4 4)
                            Cell.Deceased
                            Cell.fateOf
                            block
                in
                Maybe.map Grid.run grid
                    |> Expect.equal grid
            )
        , test "Oscillator: blinker"
            (\_ ->
                let
                    block : List Cell.State
                    block =
                        [ [ d, d, d, d, d ]
                        , [ d, d, d, d, d ]
                        , [ d, l, l, l, d ]
                        , [ d, d, d, d, d ]
                        , [ d, d, d, d, d ]
                        ]
                            |> List.concat

                    origin : Maybe (Grid.Grid Cell.State)
                    origin =
                        Grid.makeFromList
                            (Dimension.make 5 5)
                            Cell.Deceased
                            Cell.fateOf
                            block

                    intermediate : Maybe (Grid.Grid Cell.State)
                    intermediate =
                        [ [ d, d, d, d, d ]
                        , [ d, d, l, d, d ]
                        , [ d, d, l, d, d ]
                        , [ d, d, l, d, d ]
                        , [ d, d, d, d, d ]
                        ]
                            |> List.concat
                            |> Grid.makeFromList
                                (Dimension.make 5 5)
                                Cell.Deceased
                                Cell.fateOf

                    target : Maybe (Grid.Grid Cell.State)
                    target =
                        [ [ d, d, d, d, d ]
                        , [ d, d, d, d, d ]
                        , [ d, l, l, l, d ]
                        , [ d, d, d, d, d ]
                        , [ d, d, d, d, d ]
                        ]
                            |> List.concat
                            |> Grid.makeFromList
                                (Dimension.make 5 5)
                                Cell.Deceased
                                Cell.fateOf
                in
                Maybe.map Grid.run origin
                    |> Expect.all
                        [ Expect.equal intermediate
                        , \asset ->
                            Maybe.map Grid.run asset
                                |> Expect.equal target

                        -- this lambda's quite retard, is there a better syntax?
                        ]
            )
        , test "Oscillator: beacon"
            (\_ ->
                let
                    block : List Cell.State
                    block =
                        [ [ d, d, d, d, d, d ]
                        , [ d, l, l, d, d, d ]
                        , [ d, l, l, d, d, d ]
                        , [ d, d, d, l, l, d ]
                        , [ d, d, d, l, l, d ]
                        , [ d, d, d, d, d, d ]
                        ]
                            |> List.concat

                    origin : Maybe (Grid.Grid Cell.State)
                    origin =
                        Grid.makeFromList
                            (Dimension.make 6 6)
                            Cell.Deceased
                            Cell.fateOf
                            block

                    intermediate : Maybe (Grid.Grid Cell.State)
                    intermediate =
                        [ [ d, d, d, d, d, d ]
                        , [ d, l, l, d, d, d ]
                        , [ d, l, d, d, d, d ]
                        , [ d, d, d, d, l, d ]
                        , [ d, d, d, l, l, d ]
                        , [ d, d, d, d, d, d ]
                        ]
                            |> List.concat
                            |> Grid.makeFromList
                                (Dimension.make 6 6)
                                Cell.Deceased
                                Cell.fateOf

                    target : Maybe (Grid.Grid Cell.State)
                    target =
                        origin
                in
                Maybe.map Grid.run origin
                    |> Expect.all
                        [ Expect.equal intermediate
                        , \asset ->
                            Maybe.map Grid.run asset
                                |> Expect.equal target

                        -- this lambda's quite retard, is there a better syntax?
                        ]
            )
        ]


basicArrangement : Test
basicArrangement =
    let
        l =
            Cell.Live

        d =
            Cell.Deceased
    in
    describe
        """
            Test basic arrangement
            """
        [ test "A line of 4 live elements shrink to a block"
            (\_ ->
                let
                    skycrapper : Maybe (Grid.Grid Cell.State)
                    skycrapper =
                        [ [ d, l ]
                        , [ d, l ]
                        , [ d, l ]
                        , [ d, l ]
                        ]
                            |> List.concat
                            |> Grid.makeFromList
                                (Dimension.make 4 2)
                                Cell.Deceased
                                Cell.fateOf

                    collapsed : Maybe (Grid.Grid Cell.State)
                    collapsed =
                        [ [ d, d ]
                        , [ l, l ]
                        , [ l, l ]
                        , [ d, d ]
                        ]
                            |> List.concat
                            |> Grid.makeFromList
                                (Dimension.make 4 2)
                                Cell.Deceased
                                Cell.fateOf
                in
                Maybe.map Grid.run skycrapper
                    |> Expect.equal collapsed
            )
        ]


cellInRectangularSandbox : Test
cellInRectangularSandbox =
    let
        l =
            Cell.Live

        d =
            Cell.Deceased
    in
    describe
        """
            See how patterns are evolving in non-squared sandboxes
            """
        [ test "Still life: block"
            (\_ ->
                let
                    block : List Cell.State
                    block =
                        [ [ d, d, d, d ]
                        , [ d, l, l, d ]
                        , [ d, l, l, d ]
                        , [ d, d, d, d ]
                        ]
                            |> List.concat

                    grid : Maybe (Grid.Grid Cell.State)
                    grid =
                        Grid.makeFromList
                            (Dimension.make 4 4)
                            Cell.Deceased
                            Cell.fateOf
                            block
                in
                Maybe.map Grid.run grid
                    |> Expect.equal grid
            )
        , test "Oscillator: blinker"
            (\_ ->
                let
                    block : List Cell.State
                    block =
                        [ [ d, d, d, d, d ]
                        , [ d, l, l, l, d ]
                        , [ d, d, d, d, d ]
                        ]
                            |> List.concat

                    origin : Maybe (Grid.Grid Cell.State)
                    origin =
                        Grid.makeFromList
                            (Dimension.make 5 5)
                            Cell.Deceased
                            Cell.fateOf
                            block

                    intermediate : Maybe (Grid.Grid Cell.State)
                    intermediate =
                        [ [ d, d, l, d, d ]
                        , [ d, d, l, d, d ]
                        , [ d, d, l, d, d ]
                        ]
                            |> List.concat
                            |> Grid.makeFromList
                                (Dimension.make 5 5)
                                Cell.Deceased
                                Cell.fateOf

                    target : Maybe (Grid.Grid Cell.State)
                    target =
                        [ [ d, d, d, d, d ]
                        , [ d, l, l, l, d ]
                        , [ d, d, d, d, d ]
                        ]
                            |> List.concat
                            |> Grid.makeFromList
                                (Dimension.make 5 5)
                                Cell.Deceased
                                Cell.fateOf
                in
                Maybe.map Grid.run origin
                    |> Expect.all
                        [ Expect.equal intermediate
                        , \asset ->
                            Maybe.map Grid.run asset
                                |> Expect.equal target

                        -- this lambda's quite retard, is there a better syntax?
                        ]
            )
        , test "Oscillator: beacon"
            (\_ ->
                let
                    block : List Cell.State
                    block =
                        [ [ d, d, d, d, l ]
                        , [ l, d, d, l, l ]
                        , [ d, d, d, d, l ]
                        , [ l, d, d, l, l ]
                        ]
                            |> List.concat

                    origin : Maybe (Grid.Grid Cell.State)
                    origin =
                        Grid.makeFromList
                            (Dimension.make 5 5)
                            Cell.Deceased
                            Cell.fateOf
                            block

                    intermediate : Maybe (Grid.Grid Cell.State)
                    intermediate =
                        [ [ d, d, d, d, l ]
                        , [ l, d, d, l, d ]
                        , [ d, d, d, d, d ]
                        , [ l, d, d, l, l ]
                        ]
                            |> List.concat
                            |> Grid.makeFromList
                                (Dimension.make 5 5)
                                Cell.Deceased
                                Cell.fateOf

                    target : Maybe (Grid.Grid Cell.State)
                    target =
                        origin
                in
                Maybe.map Grid.run origin
                    |> Expect.all
                        [ Expect.equal intermediate
                        , \asset ->
                            Maybe.map Grid.run asset
                                |> Expect.equal target

                        -- this lambda's quite retard, is there a better syntax?
                        ]
            )
        ]
