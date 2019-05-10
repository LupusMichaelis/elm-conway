module Dict.NonemptyTests exposing (factoryTests)

import Basic
import Dict exposing (Dict)
import Dict.Nonempty exposing (Nonempty)
import Expect exposing (Expectation)
import List.Nonempty
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
        , test "One element"
            (\_ ->
                [ ( 0, "zero" ) ]
                    |> Dict.fromList
                    |> Dict.Nonempty.fromDict
                    |> Maybe.map Dict.Nonempty.size
                    |> Expect.equal (Just 1)
            )
        , test "Two element"
            (\_ ->
                [ ( 0, "zero" )
                , ( 1, "one" )
                ]
                    |> Dict.fromList
                    |> Dict.Nonempty.fromDict
                    |> Maybe.map Dict.Nonempty.size
                    |> Expect.equal (Just 2)
            )
        , test "Two element, inverted"
            (\_ ->
                [ ( 1, "one" )
                , ( 0, "zero" )
                ]
                    |> Dict.fromList
                    |> Dict.Nonempty.fromDict
                    |> Maybe.map Dict.Nonempty.size
                    |> Expect.equal (Just 2)
            )
        , test "Singleton"
            (\_ ->
                Dict.singleton 0 "zero"
                    |> Dict.Nonempty.fromDict
                    |> Maybe.map Dict.Nonempty.size
                    |> Expect.equal (Just 1)
            )
        , test "Non empty list"
            (\_ ->
                List.Nonempty.fromList [ ( 0, "zero" ) ]
                    |> Maybe.map Dict.Nonempty.fromNonemptyList
                    |> Maybe.map Dict.Nonempty.size
                    |> Expect.equal (Just 1)
            )
        ]
