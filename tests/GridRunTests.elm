module GridRunTests exposing
    ( cellForetoldFateTests
    , cellInRectangularSandbox
    )

import Grid
import Grid.Cell

import Array

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

cellForetoldFateTests: Test
cellForetoldFateTests =
    let
        l = Grid.Cell.Live
        d = Grid.Cell.Deceased
    in
        describe
            """
            Prophecies shall be fulfilled
            (https://en.wikipedia.org/wiki/Conway's_Game_of_Life#Examples_of_patterns)
            """
            [ test "Still life: block"
                (\_ ->
                    let
                        block: List Grid.Cell.State
                        block =
                            [ d, d, d, d
                            , d, l, l, d
                            , d, l, l, d
                            , d, d, d, d
                            ]

                        grid: Maybe Grid.Grid
                        grid =
                            Grid.makeFromList
                                (Grid.makeDimension 4 4)
                                block

                    in
                        Maybe.map Grid.run grid
                            |> Expect.equal grid
                )
            , test "Oscillator: blinker"
                (\_ ->
                    let
                        block: List Grid.Cell.State
                        block =
                            [ d, d, d, d, d
                            , d, d, d, d, d
                            , d, l, l, l, d
                            , d, d, d, d, d
                            , d, d, d, d, d
                            ]

                        origin: Maybe Grid.Grid
                        origin =
                            Grid.makeFromList
                                (Grid.makeDimension 5 5)
                                block

                        intermediate:  Maybe Grid.Grid
                        intermediate =
                            [ d, d, d, d, d
                            , d, d, l, d, d
                            , d, d, l, d, d
                            , d, d, l, d, d
                            , d, d, d, d, d
                            ]
                            |> Grid.makeFromList
                                (Grid.makeDimension 5 5)

                        target:  Maybe Grid.Grid
                        target =
                            [ d, d, d, d, d
                            , d, d, d, d, d
                            , d, l, l, l, d
                            , d, d, d, d, d
                            , d, d, d, d, d
                            ]
                            |> Grid.makeFromList
                                (Grid.makeDimension 5 5)

                    in
                        Maybe.map Grid.run origin
                            |> Expect.all
                                [ Expect.equal intermediate
                                , (\asset -> Maybe.map Grid.run asset
                                    |> Expect.equal target) -- this lambda's quite retard, is there a better syntax?
                                ]
                )
            , test "Oscillator: beacon"
                (\_ ->
                    let
                        block: List Grid.Cell.State
                        block =
                            [ d, d, d, d, d, d
                            , d, l, l, d, d, d
                            , d, l, l, d, d, d
                            , d, d, d, l, l, d
                            , d, d, d, l, l, d
                            , d, d, d, d, d, d
                            ]

                        origin: Maybe Grid.Grid
                        origin =
                            Grid.makeFromList
                                (Grid.makeDimension 5 5)
                                block

                        intermediate:  Maybe Grid.Grid
                        intermediate =
                            [ d, d, d, d, d, d
                            , d, l, l, d, d, d
                            , d, l, d, d, d, d
                            , d, d, d, d, l, d
                            , d, d, d, l, l, d
                            , d, d, d, d, d, d
                            ]
                            |> Grid.makeFromList
                                (Grid.makeDimension 5 5)

                        target:  Maybe Grid.Grid
                        target =
                            origin

                    in
                        Maybe.map Grid.run origin
                            |> Expect.all
                                [ Expect.equal intermediate
                                , (\asset -> Maybe.map Grid.run asset
                                    |> Expect.equal target) -- this lambda's quite retard, is there a better syntax?
                                ]
                )
            ]

cellInRectangularSandbox: Test
cellInRectangularSandbox =
    let
        l = Grid.Cell.Live
        d = Grid.Cell.Deceased
    in
        describe
            """
            See how patterns are evolving in non-squared sandboxes
            """
            [ test "Still life: block"
                (\_ ->
                    let
                        block: List Grid.Cell.State
                        block =
                            [ d, d, d, d, d, d, d
                            , d, l, l, d, d, d, d
                            , d, l, l, d, d, d, d
                            , d, d, d, d, d, d, d
                            ]

                        grid: Maybe Grid.Grid
                        grid =
                            Grid.makeFromList
                                (Grid.makeDimension 4 4)
                                block

                    in
                        Maybe.map Grid.run grid
                            |> Expect.equal grid
                )
            , test "Oscillator: blinker"
                (\_ ->
                    let
                        block: List Grid.Cell.State
                        block =
                            [ d, d, d, d, d
                            , d, l, l, l, d
                            , d, d, d, d, d
                            ]

                        origin: Maybe Grid.Grid
                        origin =
                            Grid.makeFromList
                                (Grid.makeDimension 5 5)
                                block

                        intermediate:  Maybe Grid.Grid
                        intermediate =
                            [ d, d, l, d, d
                            , d, d, l, d, d
                            , d, d, l, d, d
                            ]
                            |> Grid.makeFromList
                                (Grid.makeDimension 5 5)

                        target:  Maybe Grid.Grid
                        target =
                            [ d, d, d, d, d
                            , d, l, l, l, d
                            , d, d, d, d, d
                            ]
                            |> Grid.makeFromList
                                (Grid.makeDimension 5 5)

                    in
                        Maybe.map Grid.run origin
                            |> Expect.all
                                [ Expect.equal intermediate
                                , (\asset -> Maybe.map Grid.run asset
                                    |> Expect.equal target) -- this lambda's quite retard, is there a better syntax?
                                ]
                )
            , test "Oscillator: beacon"
                (\_ ->
                    let
                        block: List Grid.Cell.State
                        block =
                            [ d, d, d, d
                            , l, l, d, d
                            , l, l, d, d
                            , d, d, l, l
                            , d, d, l, l
                            ]

                        origin: Maybe Grid.Grid
                        origin =
                            Grid.makeFromList
                                (Grid.makeDimension 5 5)
                                block

                        intermediate:  Maybe Grid.Grid
                        intermediate =
                            [ d, d, d, d
                            , l, l, d, d
                            , l, d, d, d
                            , d, d, d, l
                            , d, d, l, l
                            ]
                            |> Grid.makeFromList
                                (Grid.makeDimension 5 5)

                        target:  Maybe Grid.Grid
                        target =
                            origin

                    in
                        Maybe.map Grid.run origin
                            |> Expect.all
                                [ Expect.equal intermediate
                                , (\asset -> Maybe.map Grid.run asset
                                    |> Expect.equal target) -- this lambda's quite retard, is there a better syntax?
                                ]
                )
            ]
