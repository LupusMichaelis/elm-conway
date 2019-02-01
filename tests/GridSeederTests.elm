module GridSeederTests exposing
    ( resizingTests
    )

import Grid
import Grid.Cell
import Seeder

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

resizingTests: Test
resizingTests =
    let
        l = Grid.Cell.Live
        d = Grid.Cell.Deceased
    in
        describe "Test resizing grid"
            [ test "Test bug of odd population, stabilized, resized with live cells and get shuffled"
                (\_ ->
                    let
                        dimension: Grid.Dimension
                        dimension =
                                Grid.makeDimension 10 10

                        original: Grid.Grid
                        original =
                            Grid.generate
                                dimension
                                Seeder.evenAreLive

                        stepZero: Maybe Grid.Grid
                        stepZero =
                            [ d, d, d, d, d, d, d, d, d, d
                            , l, l, l, l, l, l, l, l, l, l
                            , d, d, d, d, d, d, d, d, d, d
                            , l, l, l, l, l, l, l, l, l, l
                            , d, d, d, d, d, d, d, d, d, d
                            , l, l, l, l, l, l, l, l, l, l
                            , d, d, d, d, d, d, d, d, d, d
                            , l, l, l, l, l, l, l, l, l, l
                            , d, d, d, d, d, d, d, d, d, d
                            , l, l, l, l, l, l, l, l, l, l
                            ]
                                |> Grid.makeFromList dimension

                        stepOne: Maybe Grid.Grid
                        stepOne =
                            [ d, d, d, d, d, d, d, d, d, d
                            , d, l, l, l, l, l, l, l, l, d
                            , d, d, d, d, d, d, d, d, d, d
                            , d, l, l, l, l, l, l, l, l, d
                            , d, d, d, d, d, d, d, d, d, d
                            , d, l, l, l, l, l, l, l, l, d
                            , d, d, d, d, d, d, d, d, d, d
                            , d, l, l, l, l, l, l, l, l, d
                            , d, d, d, d, d, d, d, d, d, d
                            , d, l, l, l, l, l, l, l, l, d
                            ]
                                |> Grid.makeFromList dimension

                        stepTwo: Maybe Grid.Grid
                        stepTwo =
                            [ d, d, d, d, d, d, d, d, d, d
                            , d, d, l, l, l, l, l, l, d, d
                            , d, d, d, d, d, d, d, d, d, d
                            , d, d, l, l, l, l, l, l, d, d
                            , d, d, d, d, d, d, d, d, d, d
                            , d, d, l, l, l, l, l, l, d, d
                            , d, d, d, d, d, d, d, d, d, d
                            , d, d, l, l, l, l, l, l, d, d
                            , d, d, d, d, d, d, d, d, d, d
                            , d, d, l, l, l, l, l, l, d, d
                            ]
                                |> Grid.makeFromList dimension

                    in
                        original
                            |> Expect.all
                                [ (\asset ->
                                    asset
                                        |> Just
                                        |> Expect.equal stepZero
                                  )
                                ]
                )
            ]
