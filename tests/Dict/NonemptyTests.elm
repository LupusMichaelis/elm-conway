module Dict.NonemptyTests exposing (factoryTests)

import Basic
import Dict exposing (Dict)
import Dict.Nonempty exposing (Nonempty)
import Expect exposing (Expectation)
import Test exposing (..)


factoryTests : Test
factoryTests =
    describe "Nonempty Dict Factories"
        [ test "From empty dict, expect nothing"
            (\_ ->
                []
                    |> Dict.fromList
                    |> Dict.Nonempty.fromDict
                    |> Expect.equal Nothing
            )
        , test "From empty list, expect nothing"
            (\_ ->
                []
                    |> Dict.Nonempty.fromList
                    |> Expect.equal Nothing
            )
        ]
