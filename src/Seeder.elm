module Seeder exposing
    ( Blinker(..)
    , Type(..)
    , allDeceased
    , allLive
    , battlefield
    , blinker
    , evenAreLive
    , getCatalog
    , getDefault
    , getDefaultKey
    , getDefaultValue
    , oddAreLive
    )

import Cell
import Dict.Nonempty
import Dimension
import List.Nonempty
import Position


type Type state
    = Value state
    | Index (Int -> state)
    | Position (Position.Two -> state)
    | Dimension (Dimension.Two -> Position.Two -> state)


type Blinker
    = Vertical
    | Horizontal
    | Alternating Blinker Blinker


getCatalog : Dict.Nonempty.Nonempty Int ( String, Type Cell.State )
getCatalog =
    [ ( "All live cells", allLive )
    , ( "All deceased cells", allDeceased )
    , ( "Odd cells are live, other empty", oddAreLive )
    , ( "Even cells are live, other empty", evenAreLive )
    , ( "A battlefield with a living cell every 3 and 5 cell", battlefield )
    , ( "Place blinkers horizontally", blinker Horizontal )
    , ( "Place blinkers vertically", blinker Vertical )
    , ( "Place blinkers horizontally, then vertically, alternatively"
      , blinker <| Alternating Vertical Horizontal
      )
    ]
        |> List.Nonempty.Nonempty
            ( "All live cells", allLive )
        |> List.Nonempty.indexedMap Tuple.pair
        |> Dict.Nonempty.fromNonemptyList


getDefault : ( Int, ( String, Type Cell.State ) )
getDefault =
    let
        catalog =
            getCatalog

        (Dict.Nonempty.Nonempty k v d) =
            catalog
    in
    catalog
        |> Dict.Nonempty.get 2
        |> Maybe.map (Tuple.pair 2)
        |> Maybe.withDefault ( k, v )


getDefaultValue : ( String, Type Cell.State )
getDefaultValue =
    getDefault |> Tuple.second


getDefaultKey : Int
getDefaultKey =
    getDefault |> Tuple.first


allLive : Type Cell.State
allLive =
    Value Cell.Live


allDeceased : Type Cell.State
allDeceased =
    Value Cell.Deceased


oddAreLive : Type Cell.State
oddAreLive =
    Index
        (\idx ->
            if modBy 2 idx == 0 then
                Cell.Deceased

            else
                Cell.Live
        )


evenAreLive : Type Cell.State
evenAreLive =
    Index
        (\idx ->
            if modBy 2 idx == 0 then
                Cell.Live

            else
                Cell.Deceased
        )


battlefield : Type Cell.State
battlefield =
    Index
        (\idx ->
            if
                modBy 3 idx
                    == 0
                    || modBy 5 idx
                    == 0
            then
                Cell.Live

            else
                Cell.Deceased
        )


blinker : Blinker -> Type Cell.State
blinker disposition =
    let
        horizontal : Position.Two -> Cell.State
        horizontal position =
            if
                1
                    == modBy 4 position.t
                    && ([ 0, 1, 2 ]
                            |> List.map ((==) (modBy 4 position.l))
                            |> List.foldl (||) False
                       )
            then
                Cell.Live

            else
                Cell.Deceased

        vertical : Position.Two -> Cell.State
        vertical position =
            if
                1
                    == modBy 4 position.l
                    && ([ 0, 1, 2 ]
                            |> List.map ((==) (modBy 4 position.t))
                            |> List.foldl (||) False
                       )
            then
                Cell.Live

            else
                Cell.Deceased

        alternating : Blinker -> Blinker -> Position.Two -> Cell.State
        alternating a b position =
            let
                f =
                    if modBy 6 position.l < 4 then
                        case a of
                            Horizontal ->
                                horizontal

                            Vertical ->
                                vertical

                            Alternating an bn ->
                                alternating an bn

                    else
                        case b of
                            Horizontal ->
                                horizontal

                            Vertical ->
                                vertical

                            Alternating an bn ->
                                alternating an bn
            in
            f position
    in
    Position <|
        case disposition of
            Horizontal ->
                horizontal

            Vertical ->
                vertical

            Alternating a b ->
                alternating a b
