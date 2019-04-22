module Seeder exposing
    ( Type(..)
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
import Dict exposing (Dict)
import Dimension
import Position


type Type state
    = Value state
    | Index (Int -> state)
    | Position (Position.Two -> state)
    | Dimension (Dimension.Two -> Position.Two -> state)


getCatalog : Dict Int ( String, Type Cell.State )
getCatalog =
    Dict.fromList <|
        List.indexedMap Tuple.pair <|
            [ ( "All live cells", allLive )
            , ( "All deceased cells", allDeceased )
            , ( "Odd cells are live, other empty", oddAreLive )
            , ( "Even cells are live, other empty", evenAreLive )
            , ( "A battlefield with a living cell every 3 and 5 cell", battlefield )
            ]


getDefault : ( Int, ( String, Type Cell.State ) )
getDefault =
    getCatalog
        |> Dict.get 2
        |> Maybe.map (Tuple.pair 2)
        |> Maybe.withDefault ( 0, ( "All live cells", allLive ) )


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


blinker : Type Cell.State
blinker =
    Index
        (\idx ->
            if idx == 15 + 0 then
                Cell.Live

            else if idx == 15 + 1 then
                Cell.Live

            else if idx == 15 + 2 then
                Cell.Live

            else
                Cell.Deceased
        )
