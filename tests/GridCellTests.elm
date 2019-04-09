module GridCellTests exposing
    ( cellFateTests
    , cellStateMachineTests
    )

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
                    []
                    |> Expect.true "expected to drop dead"
            )
        , test "I'm not feeling so well, Mr Stark X-P"
            (\_ ->
                Cell.shouldACellDie
                    (List.repeat 1 Cell.Live)
                    |> Expect.true "expected to drop dead, even with someone around"
            )
        , test "I'm not feeling."
            (\_ ->
                Cell.shouldACellResurrect
                    []
                    |> Expect.false "expected to stay dead"
            )
        , test "Praying cells"
            (\_ ->
                Cell.shouldACellResurrect
                    (List.repeat 3 Cell.Live)
                    |> Expect.true "expected to do a Jesus"
            )
        , test "Two praying cells, with one dead"
            (\_ ->
                let
                    chorus : List Cell.State
                    chorus =
                        List.repeat 2 Cell.Live
                            |> (::) Cell.Deceased
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
                Cell.fateOf Cell.Deceased []
                    |> Expect.equal Cell.Deceased
            )
        , test "Alone in the void"
            (\_ ->
                Cell.fateOf Cell.Live []
                    |> Expect.equal Cell.Deceased
            )
        , test "Chocked to death"
            (\_ ->
                Cell.fateOf Cell.Live
                    (List.repeat 5 Cell.Live)
                    |> Expect.equal Cell.Deceased
            )
        , test "Revived"
            (\_ ->
                Cell.fateOf Cell.Deceased
                    (List.repeat 3 Cell.Live)
                    |> Expect.equal Cell.Live
            )
        ]
