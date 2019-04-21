module ManyStateGridTests exposing (manyStates)

import Cell
import Dimension
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Grid
import Position
import Test exposing (..)


type MockState
    = MockState Int


mockFate : MockState -> List MockState -> MockState
mockFate _ _ =
    MockState 0


mockSeeder : Int -> MockState
mockSeeder =
    MockState


cmpMockState : MockState -> MockState -> Order
cmpMockState (MockState lhs) (MockState rhs) =
    compare lhs rhs


manyStates : Test
manyStates =
    describe "Values held by grid"
        [ test "Have squared grid"
            (\_ ->
                let
                    expected =
                        [ [ MockState 0, MockState 1, MockState 2 ]
                        , [ MockState 3, MockState 4, MockState 5 ]
                        , [ MockState 6, MockState 7, MockState 8 ]
                        ]
                            |> List.concat
                            |> Grid.makeFromList
                                (Dimension.Two 3 3)
                                (MockState 0)
                                mockFate
                in
                Grid.generate
                    (Dimension.Two 3 3)
                    (MockState 0)
                    mockFate
                    mockSeeder
                    |> Just
                    |> Expect.equal expected
            )
        , test "Have horizontal rectangular grid"
            (\_ ->
                let
                    expected =
                        [ [ MockState 0, MockState 1, MockState 2 ]
                        , [ MockState 3, MockState 4, MockState 5 ]
                        ]
                            |> List.concat
                            |> Grid.makeFromList
                                (Dimension.Two 2 3)
                                (MockState 0)
                                mockFate
                in
                Grid.generate
                    (Dimension.Two 2 3)
                    (MockState 0)
                    mockFate
                    mockSeeder
                    |> Just
                    |> Expect.equal expected
            )
        , test "Have vertical rectangular grid"
            (\_ ->
                let
                    expected =
                        [ [ MockState 0, MockState 1 ]
                        , [ MockState 2, MockState 3 ]
                        , [ MockState 4, MockState 5 ]
                        ]
                            |> List.concat
                            |> Grid.makeFromList
                                (Dimension.Two 3 2)
                                (MockState 0)
                                mockFate
                in
                Grid.generate
                    (Dimension.Two 3 2)
                    (MockState 0)
                    mockFate
                    mockSeeder
                    |> Just
                    |> Expect.equal expected
            )
        , test "Check neighbours in a squared area"
            (\_ ->
                List.range 0 (10 * 10 - 1)
                    |> List.map MockState
                    |> Grid.makeFromList
                        (Dimension.Two 10 10)
                        (MockState 0)
                        mockFate
                    |> Maybe.map
                        (Expect.all
                            [ \asset ->
                                Grid.getNeighbourPositions
                                    (Position.make 1024 523)
                                    asset.dimension
                                    |> List.map (Grid.getStateAt asset)
                                    |> List.sortWith cmpMockState
                                    |> Expect.equal
                                        []
                            , \asset ->
                                Grid.getNeighbourPositions
                                    (Position.make 0 0)
                                    asset.dimension
                                    |> List.map (Grid.getStateAt asset)
                                    |> List.sortWith cmpMockState
                                    |> Expect.equal
                                        [ MockState 1
                                        , MockState 10
                                        , MockState 11
                                        ]
                            , \asset ->
                                Grid.getNeighbourPositions
                                    (Position.make (10 - 1) (10 - 1))
                                    asset.dimension
                                    |> List.map (Grid.getStateAt asset)
                                    |> List.sortWith cmpMockState
                                    |> Expect.equal
                                        [ MockState 88
                                        , MockState 89
                                        , MockState 98
                                        ]
                            , \asset ->
                                Grid.getNeighbourPositions
                                    (Position.make 5 (10 - 1))
                                    asset.dimension
                                    |> List.map (Grid.getStateAt asset)
                                    |> List.sortWith cmpMockState
                                    |> Expect.equal
                                        [ MockState 48
                                        , MockState 49
                                        , MockState 58
                                        , MockState 68
                                        , MockState 69
                                        ]
                            , \asset ->
                                Grid.getNeighbourPositions
                                    (Position.make 5 7)
                                    asset.dimension
                                    |> List.map (Grid.getStateAt asset)
                                    |> List.sortWith cmpMockState
                                    |> Expect.equal
                                        [ MockState 46
                                        , MockState 47
                                        , MockState 48
                                        , MockState 56
                                        , MockState 58
                                        , MockState 66
                                        , MockState 67
                                        , MockState 68
                                        ]
                            ]
                        )
                    |> Maybe.withDefault (Expect.fail "Grid couldn't be generated")
            )
        , test "Check neighbours in a horizontal rectangular area"
            (\_ ->
                List.range 0 (8 * 10 - 1)
                    |> List.map MockState
                    |> Grid.makeFromList
                        (Dimension.Two 8 10)
                        (MockState 0)
                        mockFate
                    |> Maybe.map
                        (Expect.all
                            [ \asset ->
                                Grid.getNeighbourPositions
                                    (Position.make 1024 523)
                                    asset.dimension
                                    |> List.map (Grid.getStateAt asset)
                                    |> List.sortWith cmpMockState
                                    |> Expect.equal
                                        []
                            , \asset ->
                                Grid.getNeighbourPositions
                                    (Position.make 0 0)
                                    asset.dimension
                                    |> List.map (Grid.getStateAt asset)
                                    |> List.sortWith cmpMockState
                                    |> Expect.equal
                                        [ MockState 1
                                        , MockState 10
                                        , MockState 11
                                        ]
                            , \asset ->
                                Grid.getNeighbourPositions
                                    (Position.make (8 - 1) (10 - 1))
                                    asset.dimension
                                    |> List.map (Grid.getStateAt asset)
                                    |> List.sortWith cmpMockState
                                    |> Expect.equal
                                        [ MockState 68
                                        , MockState 69
                                        , MockState 78
                                        ]
                            , \asset ->
                                Grid.getNeighbourPositions
                                    (Position.make 5 (10 - 1))
                                    asset.dimension
                                    |> List.map (Grid.getStateAt asset)
                                    |> List.sortWith cmpMockState
                                    |> Expect.equal
                                        [ MockState 48
                                        , MockState 49
                                        , MockState 58
                                        , MockState 68
                                        , MockState 69
                                        ]
                            , \asset ->
                                Grid.getNeighbourPositions
                                    (Position.make 5 6)
                                    asset.dimension
                                    |> List.map (Grid.getStateAt asset)
                                    |> List.sortWith cmpMockState
                                    |> Expect.equal
                                        [ MockState 45
                                        , MockState 46
                                        , MockState 47
                                        , MockState 55
                                        , MockState 57
                                        , MockState 65
                                        , MockState 66
                                        , MockState 67
                                        ]
                            ]
                        )
                    |> Maybe.withDefault (Expect.fail "Grid couldn't be generated")
            )
        ]
