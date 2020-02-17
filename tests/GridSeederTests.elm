module GridSeederTests exposing (resizingTests)

import Cell
import Dimension
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Grid
import Seeder
import Test exposing (..)


resizingTests : Test
resizingTests =
    let
        l =
            Cell.Live

        d =
            Cell.Deceased
    in
    describe "Test resizing grid with seeder"
        [ test "Test bug of odd population, stabilized, resized with live cells and get shuffled"
            (\_ ->
                let
                    dimension : Dimension.Two
                    dimension =
                        Dimension.make 3 5

                    original : Grid.Grid Cell.State
                    original =
                        Grid.generate
                            dimension
                            Cell.Deceased
                            Cell.b3s23
                            Seeder.oddAreLive

                    stepZero : Maybe (Grid.Grid Cell.State)
                    stepZero =
                        [ [ d, l, d, l, d ]
                        , [ l, d, l, d, l ]
                        , [ d, l, d, l, d ]
                        ]
                            |> List.concat
                            |> Grid.makeFromList dimension
                                Cell.Deceased
                                Cell.b3s23

                    stepOne : Maybe (Grid.Grid Cell.State)
                    stepOne =
                        [ [ d, l, l, l, d ]
                        , [ l, d, d, d, l ]
                        , [ d, l, l, l, d ]
                        ]
                            |> List.concat
                            |> Grid.makeFromList dimension
                                Cell.Deceased
                                Cell.b3s23

                    stepTwo : Maybe (Grid.Grid Cell.State)
                    stepTwo =
                        [ [ d, l, l, l, d ]
                        , [ l, d, d, d, l ]
                        , [ d, l, l, l, d ]
                        ]
                            |> List.concat
                            |> Grid.makeFromList dimension
                                Cell.Deceased
                                Cell.b3s23

                    stepThree : Maybe (Grid.Grid Cell.State)
                    stepThree =
                        [ [ d, l, l ]
                        , [ l, d, d ]
                        , [ d, l, l ]
                        ]
                            |> List.concat
                            |> Grid.makeFromList (Dimension.make 3 3)
                                Cell.Deceased
                                Cell.b3s23

                    stepFour : Maybe (Grid.Grid Cell.State)
                    stepFour =
                        [ [ d, l, l ]
                        , [ l, d, d ]
                        , [ d, l, l ]
                        , [ l, d, l ]
                        , [ d, l, d ]
                        , [ l, d, l ]
                        ]
                            |> List.concat
                            |> Grid.makeFromList (Dimension.make 6 3)
                                Cell.Deceased
                                Cell.b3s23
                in
                original
                    |> Expect.all
                        [ \asset ->
                            asset
                                |> Just
                                |> Expect.equal stepZero
                        , \asset ->
                            asset
                                |> Grid.run
                                |> Just
                                |> Expect.equal stepOne
                        , \asset ->
                            asset
                                |> Grid.run
                                |> Grid.run
                                |> Just
                                |> Expect.equal stepTwo
                        , \asset ->
                            asset
                                |> Grid.run
                                |> Grid.run
                                |> (\grid ->
                                        Grid.makeFromGridThenResize
                                            grid
                                            (Dimension.make 3 3)
                                            Seeder.oddAreLive
                                   )
                                |> Just
                                |> Expect.equal stepThree
                        , \asset ->
                            asset
                                |> Grid.run
                                |> Grid.run
                                |> (\grid ->
                                        Grid.makeFromGridThenResize
                                            grid
                                            (Dimension.make 3 3)
                                            Seeder.oddAreLive
                                   )
                                |> (\grid ->
                                        Grid.makeFromGridThenResize
                                            grid
                                            (Dimension.make 6 3)
                                            Seeder.oddAreLive
                                   )
                                |> Just
                                |> Expect.equal stepFour
                        ]
            )
        ]
