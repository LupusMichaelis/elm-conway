module Dict.NonEmptyTests exposing (factoryTests)

import Basic
import Dict exposing (Dict)
import Dict.NonEmpty exposing (NonEmpty)
import Expect exposing (Expectation)
import Test exposing (..)


factoryTests : Test
factoryTests =
    describe "NonEmpty Dict Factories"
        [ test "From empty dict, expect nothing"
            (\_ ->
                []
                    |> Dict.fromList
                    |> Dict.NonEmpty.fromDict
                    |> Expect.equal Nothing
            )
        , test "From empty list, expect nothing"
            (\_ ->
                []
                    |> Dict.NonEmpty.fromList
                    |> Expect.equal Nothing
            )
        ]
