module GridCellTests exposing
    ( cellFateTests
    , cellStateMachineTests
    )

import Array exposing (Array)
import Cell
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


cellStateMachineTests : Test
cellStateMachineTests =
    describe "Die cell! Die!!!!!!"
        [ test "I'm feeling lonely."
            (\_ ->
                Cell.shouldACellDie
                    Array.empty
                    |> Expect.true "expected to drop dead"
            )
        , test "I'm not feeling so well, Mr Stark X-P"
            (\_ ->
                Cell.shouldACellDie
                    (Array.repeat 1 Cell.Live)
                    |> Expect.true "expected to drop dead, even with someone around"
            )
        , test "I'm not feeling."
            (\_ ->
                Cell.shouldACellResurrect
                    Array.empty
                    |> Expect.false "expected to stay dead"
            )
        , test "Praying cells"
            (\_ ->
                Cell.shouldACellResurrect
                    (Array.repeat 3 Cell.Live)
                    |> Expect.true "expected to do a Jesus"
            )
        , test "Two praying cells, with one dead"
            (\_ ->
                let
                    chorus : Array Cell.State
                    chorus =
                        Array.repeat 2 Cell.Live
                            |> Array.push Cell.Deceased
                in
                Cell.shouldACellResurrect
                    chorus
                    |> Expect.false "expected to do not come back"
            )
        ]


cellFateTests : Test
cellFateTests =
    describe "Determine fate depending on cell's neighbourhood."
        [ test "Emptiness remains empty"
            (\_ ->
                Cell.fateOf Cell.Deceased Array.empty
                    |> Expect.equal Cell.Deceased
            )
        , test "Alone in the void"
            (\_ ->
                Cell.fateOf Cell.Live Array.empty
                    |> Expect.equal Cell.Deceased
            )
        , test "Chocked to death"
            (\_ ->
                Cell.fateOf Cell.Live
                    (Array.repeat 5 Cell.Live)
                    |> Expect.equal Cell.Deceased
            )
        , test "Revived"
            (\_ ->
                Cell.fateOf Cell.Deceased
                    (Array.repeat 3 Cell.Live)
                    |> Expect.equal Cell.Live
            )
        ]
