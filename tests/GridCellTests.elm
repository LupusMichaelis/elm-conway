module GridCellTests exposing
    ( cellFateTests
    , cellStateMachineTests
    )

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Grid.Cell
import Test exposing (..)


cellStateMachineTests : Test
cellStateMachineTests =
    describe "Die cell! Die!!!!!!"
        [ test "I'm feeling lonely."
            (\_ ->
                Grid.Cell.shouldACellDie
                    []
                    |> Expect.true "expected to drop dead"
            )
        , test "I'm not feeling so well, Mr Stark X-P"
            (\_ ->
                Grid.Cell.shouldACellDie
                    (List.repeat 1 Grid.Cell.Live)
                    |> Expect.true "expected to drop dead, even with someone around"
            )
        , test "I'm not feeling."
            (\_ ->
                Grid.Cell.shouldACellResurrect
                    []
                    |> Expect.false "expected to stay dead"
            )
        , test "Praying cells"
            (\_ ->
                Grid.Cell.shouldACellResurrect
                    (List.repeat 3 Grid.Cell.Live)
                    |> Expect.true "expected to do a Jesus"
            )
        , test "Two praying cells, with one dead"
            (\_ ->
                let
                    chorus : List Grid.Cell.State
                    chorus =
                        List.repeat 2 Grid.Cell.Live
                            |> (::) Grid.Cell.Deceased
                in
                Grid.Cell.shouldACellResurrect
                    chorus
                    |> Expect.false "expected to do not come back"
            )
        ]


cellFateTests : Test
cellFateTests =
    describe "Determine fate depending on cell's neighbourhood."
        [ test "Emptiness remains empty"
            (\_ ->
                Grid.Cell.fateOf Grid.Cell.Deceased []
                    |> Expect.equal Grid.Cell.Deceased
            )
        , test "Alone in the void"
            (\_ ->
                Grid.Cell.fateOf Grid.Cell.Live []
                    |> Expect.equal Grid.Cell.Deceased
            )
        , test "Chocked to death"
            (\_ ->
                Grid.Cell.fateOf Grid.Cell.Live
                    (List.repeat 5 Grid.Cell.Live)
                    |> Expect.equal Grid.Cell.Deceased
            )
        , test "Revived"
            (\_ ->
                Grid.Cell.fateOf Grid.Cell.Deceased
                    (List.repeat 3 Grid.Cell.Live)
                    |> Expect.equal Grid.Cell.Live
            )
        ]
