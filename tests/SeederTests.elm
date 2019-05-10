module SeederTests exposing
    ( catalogTests
    , seederTests
    )

import Cell
import Dict.Nonempty
import Expect exposing (Expectation)
import Position
import Seeder
import Test exposing (..)


catalogTests : Test
catalogTests =
    describe "Catalog of seeds"
        [ test "Check if we've got at least one seeder"
            (\_ ->
                Seeder.getCatalog
                    |> Dict.Nonempty.size
                    |> Expect.atLeast 1
            )
        ]


seederTests : Test
seederTests =
    describe "Seeders"
        [ test "Always live"
            (\_ ->
                Seeder.allLive
                    |> Expect.equal (Seeder.Value Cell.Live)
            )
        , test "Always dead"
            (\_ ->
                Seeder.allDeceased
                    |> Expect.equal (Seeder.Value Cell.Deceased)
            )
        , test "Odd are live"
            (\_ ->
                case Seeder.oddAreLive of
                    Seeder.Index f ->
                        f
                            |> Expect.all
                                [ (|>) 0 >> Expect.equal Cell.Deceased
                                , (|>) 1 >> Expect.equal Cell.Live
                                , (|>) 10 >> Expect.equal Cell.Deceased
                                , (|>) 11 >> Expect.equal Cell.Live
                                , (|>) 42 >> Expect.equal Cell.Deceased
                                , (|>) 33 >> Expect.equal Cell.Live
                                ]

                    _ ->
                        Expect.fail "The seeder should be index driven"
            )
        , test "Even are live"
            (\_ ->
                case Seeder.evenAreLive of
                    Seeder.Index f ->
                        f
                            |> Expect.all
                                [ (|>) 0 >> Expect.equal Cell.Live
                                , (|>) 1 >> Expect.equal Cell.Deceased
                                , (|>) 10 >> Expect.equal Cell.Live
                                , (|>) 11 >> Expect.equal Cell.Deceased
                                , (|>) 42 >> Expect.equal Cell.Live
                                , (|>) 33 >> Expect.equal Cell.Deceased
                                ]

                    _ ->
                        Expect.fail "The seeder should be index driven"
            )
        , test "Battlefield"
            (\_ ->
                case Seeder.battlefield of
                    Seeder.Index f ->
                        f
                            |> Expect.all
                                [ (|>) 0 >> Expect.equal Cell.Live
                                , (|>) 1 >> Expect.equal Cell.Deceased
                                , (|>) 2 >> Expect.equal Cell.Deceased
                                , (|>) 3 >> Expect.equal Cell.Live
                                , (|>) 4 >> Expect.equal Cell.Deceased
                                , (|>) 5 >> Expect.equal Cell.Live
                                , (|>) 6 >> Expect.equal Cell.Live
                                , (|>) 7 >> Expect.equal Cell.Deceased
                                , (|>) 8 >> Expect.equal Cell.Deceased
                                , (|>) 9 >> Expect.equal Cell.Live
                                , (|>) 10 >> Expect.equal Cell.Live
                                ]

                    _ ->
                        Expect.fail "The seeder should be index driven"
            )
        , test "Blinker placement"
            (\_ ->
                case Seeder.blinker Seeder.Horizontal of
                    Seeder.Position f ->
                        f
                            |> Expect.all
                                [ (|>) (Position.Two 0 0) >> Expect.equal Cell.Deceased
                                , (|>) (Position.Two 0 1) >> Expect.equal Cell.Deceased
                                , (|>) (Position.Two 0 2) >> Expect.equal Cell.Deceased
                                , (|>) (Position.Two 0 3) >> Expect.equal Cell.Deceased
                                , (|>) (Position.Two 1 0) >> Expect.equal Cell.Live
                                , (|>) (Position.Two 1 1) >> Expect.equal Cell.Live
                                , (|>) (Position.Two 1 2) >> Expect.equal Cell.Live
                                , (|>) (Position.Two 1 3) >> Expect.equal Cell.Deceased
                                , (|>) (Position.Two 2 0) >> Expect.equal Cell.Deceased
                                , (|>) (Position.Two 2 1) >> Expect.equal Cell.Deceased
                                , (|>) (Position.Two 2 2) >> Expect.equal Cell.Deceased
                                , (|>) (Position.Two 2 3) >> Expect.equal Cell.Deceased
                                ]

                    _ ->
                        Expect.fail "The seeder should be position driven"
            )
        , test "Blinker placed alternatively"
            (\_ ->
                case Seeder.blinker Seeder.Vertical of
                    Seeder.Position f ->
                        f
                            |> Expect.all
                                [ (|>) (Position.Two 0 0) >> Expect.equal Cell.Deceased
                                , (|>) (Position.Two 0 1) >> Expect.equal Cell.Live
                                , (|>) (Position.Two 0 2) >> Expect.equal Cell.Deceased
                                , (|>) (Position.Two 0 3) >> Expect.equal Cell.Deceased
                                , (|>) (Position.Two 1 0) >> Expect.equal Cell.Deceased
                                , (|>) (Position.Two 1 1) >> Expect.equal Cell.Live
                                , (|>) (Position.Two 1 2) >> Expect.equal Cell.Deceased
                                , (|>) (Position.Two 1 3) >> Expect.equal Cell.Deceased
                                , (|>) (Position.Two 2 0) >> Expect.equal Cell.Deceased
                                , (|>) (Position.Two 2 1) >> Expect.equal Cell.Live
                                , (|>) (Position.Two 2 2) >> Expect.equal Cell.Deceased
                                , (|>) (Position.Two 2 3) >> Expect.equal Cell.Deceased
                                ]

                    _ ->
                        Expect.fail "The seeder should be position driven"
            )
        ]
