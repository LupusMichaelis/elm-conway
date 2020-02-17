module GridCellTests exposing
    ( cellFateTests
    , cellStateMachineTests
    , rulerTests
    )

import Array exposing (Array)
import Cell
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


rulerTests : Test
rulerTests =
    describe "Apply rule"
        [ test "Rule live"
            (\_ ->
                let
                    rule : Cell.Rule Cell.State
                    rule =
                        Cell.Match [ 1 ] Cell.Live Cell.Deceased Cell.Live
                in
                Array.repeat 1 Cell.Live
                    |> Cell.abideByTheRule rule Cell.Live
                    |> Expect.equal Cell.Live
            )
        , test "Rule dead"
            (\_ ->
                let
                    rule : Cell.Rule Cell.State
                    rule =
                        Cell.Match [ 1 ] Cell.Live Cell.Deceased Cell.Live
                in
                Array.repeat 1 Cell.Live
                    |> Cell.abideByTheRule rule Cell.Deceased
                    |> Expect.equal Cell.Live
            )
        , test "Requires two live, to get live, stay dead"
            (\_ ->
                let
                    rule : Cell.Rule Cell.State
                    rule =
                        Cell.Match [ 2 ] Cell.Live Cell.Deceased Cell.Live
                in
                Array.repeat 1 Cell.Live
                    |> Cell.abideByTheRule rule Cell.Deceased
                    |> Expect.equal Cell.Deceased
            )
        , test "Requires two live, to get live, raise from the dead"
            (\_ ->
                let
                    rule : Cell.Rule Cell.State
                    rule =
                        Cell.Match [ 2 ] Cell.Live Cell.Deceased Cell.Live
                in
                Array.repeat 2 Cell.Live
                    |> Cell.abideByTheRule rule Cell.Deceased
                    |> Expect.equal Cell.Live
            )
        , test "Requires one or two live, to get live, raise from the dead"
            (\_ ->
                Cell.Match [ 1, 2 ] Cell.Live Cell.Deceased Cell.Live
                    |> Expect.all
                        [ \rule ->
                            Array.empty
                                |> Cell.abideByTheRule rule Cell.Deceased
                                |> Expect.equal Cell.Deceased
                        , \rule ->
                            Array.fromList [ Cell.Live ]
                                |> Cell.abideByTheRule rule Cell.Deceased
                                |> Expect.equal Cell.Live
                        , \rule ->
                            Array.repeat 2 Cell.Live
                                |> Cell.abideByTheRule rule Cell.Deceased
                                |> Expect.equal Cell.Live
                        , \rule ->
                            Array.repeat 3 Cell.Live
                                |> Cell.abideByTheRule rule Cell.Deceased
                                |> Expect.equal Cell.Deceased
                        ]
            )
        , test "First rule live, second die, should stay alive"
            (\_ ->
                let
                    rule : List (Cell.Rule Cell.State)
                    rule =
                        [ Cell.Match [ 1 ] Cell.Live Cell.Deceased Cell.Live
                        , Cell.Match [ 1 ] Cell.Live Cell.Deceased Cell.Deceased
                        ]
                in
                Array.repeat 1 Cell.Live
                    |> Cell.fateOf rule Cell.Live
                    |> Expect.equal Cell.Live
            )
        ]


cellStateMachineTests : Test
cellStateMachineTests =
    describe "Die cell! Die!!!!!!"
        [ test "I'm feeling lonely."
            (\_ ->
                Cell.fateOf
                    Cell.b3s23
                    Cell.Live
                    Array.empty
                    |> Expect.equal Cell.Deceased
            )
        , test "I'm not feeling so well, Mr Stark X-P"
            (\_ ->
                Cell.fateOf
                    Cell.b3s23
                    Cell.Live
                    (Array.repeat 1 Cell.Live)
                    |> Expect.equal Cell.Deceased
            )
        , test "I'm not feeling."
            (\_ ->
                Cell.fateOf
                    Cell.b3s23
                    Cell.Live
                    Array.empty
                    |> Expect.equal Cell.Deceased
            )
        , test "Praying cells for the return of Jay"
            (\_ ->
                Cell.fateOf
                    Cell.b3s23
                    Cell.Deceased
                    (Array.repeat 3 Cell.Live)
                    |> Expect.equal Cell.Live
            )
        , test "Two praying cells, with one dead"
            (\_ ->
                let
                    chorus : Array Cell.State
                    chorus =
                        Array.repeat 2 Cell.Live
                            |> Array.push Cell.Deceased
                in
                Cell.fateOf
                    Cell.b3s23
                    Cell.Deceased
                    chorus
                    |> Expect.equal Cell.Deceased
            )
        ]


cellFateTests : Test
cellFateTests =
    describe "Determine fate depending on cell's neighbourhood."
        [ test "Emptiness remains empty"
            (\_ ->
                Cell.fateOf Cell.b3s23 Cell.Deceased Array.empty
                    |> Expect.equal Cell.Deceased
            )
        , test "Alone in the void"
            (\_ ->
                Cell.fateOf
                    Cell.b3s23
                    Cell.Live
                    Array.empty
                    |> Expect.equal Cell.Deceased
            )
        , test "Chocked to death"
            (\_ ->
                Cell.fateOf
                    Cell.b3s23
                    Cell.Live
                    (Array.repeat 5 Cell.Live)
                    |> Expect.equal Cell.Deceased
            )
        , test "Revived"
            (\_ ->
                Cell.fateOf Cell.b3s23
                    Cell.Deceased
                    (Array.repeat 3 Cell.Live)
                    |> Expect.equal Cell.Live
            )
        ]
