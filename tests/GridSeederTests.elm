module GridSeederTests exposing (resizingTests)

import Cell
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
    describe "Test resizing grid"
        [ test "Test bug of odd population, stabilized, resized with live cells and get shuffled"
            (\_ ->
                let
                    dimension : Grid.Dimension
                    dimension =
                        Grid.makeDimension 10 10

                    original : Grid.Grid Cell.State
                    original =
                        Grid.generate
                            dimension
                            Cell.Deceased
                            Cell.fateOf
                            Seeder.evenAreLive

                    stepZero : Maybe (Grid.Grid Cell.State)
                    stepZero =
                        [ [ d, l, d, l, d, l, d, l, d, l ]
                        , [ d, l, d, l, d, l, d, l, d, l ]
                        , [ d, l, d, l, d, l, d, l, d, l ]
                        , [ d, l, d, l, d, l, d, l, d, l ]
                        , [ d, l, d, l, d, l, d, l, d, l ]
                        , [ d, l, d, l, d, l, d, l, d, l ]
                        , [ d, l, d, l, d, l, d, l, d, l ]
                        , [ d, l, d, l, d, l, d, l, d, l ]
                        , [ d, l, d, l, d, l, d, l, d, l ]
                        , [ d, l, d, l, d, l, d, l, d, l ]
                        ]
                            |> List.concat
                            |> Grid.makeFromList dimension
                                Cell.Deceased
                                Cell.fateOf

                    stepOne : Maybe (Grid.Grid Cell.State)
                    stepOne =
                        [ [ d, d, d, d, d, d, d, d, d, d ]
                        , [ l, l, d, l, d, l, d, l, d, l ]
                        , [ l, l, d, l, d, l, d, l, d, l ]
                        , [ l, l, d, l, d, l, d, l, d, l ]
                        , [ l, l, d, l, d, l, d, l, d, l ]
                        , [ l, l, d, l, d, l, d, l, d, l ]
                        , [ l, l, d, l, d, l, d, l, d, l ]
                        , [ l, l, d, l, d, l, d, l, d, l ]
                        , [ l, l, d, l, d, l, d, l, d, l ]
                        , [ d, d, d, d, d, d, d, d, d, d ]
                        ]
                            |> List.concat
                            |> Grid.makeFromList dimension
                                Cell.Deceased
                                Cell.fateOf

                    stepTwo : Maybe (Grid.Grid Cell.State)
                    stepTwo =
                        [ [ d, d, d, d, d, d, d, d, d, d ]
                        , [ d, d, l, l, l, l, l, l, d, d ]
                        , [ d, d, d, d, d, d, d, d, d, d ]
                        , [ d, d, l, l, l, l, l, l, d, d ]
                        , [ d, d, d, d, d, d, d, d, d, d ]
                        , [ d, d, l, l, l, l, l, l, d, d ]
                        , [ d, d, d, d, d, d, d, d, d, d ]
                        , [ d, d, l, l, l, l, l, l, d, d ]
                        , [ d, d, d, d, d, d, d, d, d, d ]
                        , [ d, d, l, l, l, l, l, l, d, d ]
                        ]
                            |> List.concat
                            |> Grid.makeFromList dimension
                                Cell.Deceased
                                Cell.fateOf
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
                        ]
            )
        ]
