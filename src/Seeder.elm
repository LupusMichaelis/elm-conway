module Seeder exposing
    ( Seeder
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


type alias Seeder =
    Int -> Cell.State


getCatalog : Dict Int ( String, Seeder )
getCatalog =
    Dict.fromList <|
        List.indexedMap Tuple.pair <|
            [ ( "All live cells", allLive )
            , ( "All deceased cells", allDeceased )
            , ( "Odd cells are live, other empty", oddAreLive )
            , ( "Even cells are live, other empty", evenAreLive )
            , ( "A battlefield with a living cell every 3 and 5 cell", battlefield )
            ]


getDefault : ( Int, ( String, Seeder ) )
getDefault =
    ( getDefaultKey, getDefaultValue )


getDefaultValue : ( String, Seeder )
getDefaultValue =
    ( "All live cells", allLive )


getDefaultKey : Int
getDefaultKey =
    0


allLive : Seeder
allLive =
    always Cell.Live


allDeceased : Seeder
allDeceased =
    always Cell.Deceased


oddAreLive : Seeder
oddAreLive idx =
    if modBy 2 idx == 0 then
        Cell.Live

    else
        Cell.Deceased


evenAreLive : Seeder
evenAreLive idx =
    if modBy 2 idx == 0 then
        Cell.Deceased

    else
        Cell.Live


battlefield : Seeder
battlefield idx =
    if
        modBy 3 idx
            == 0
            || modBy 5 idx
            == 0
    then
        Cell.Live

    else
        Cell.Deceased


blinker : Seeder
blinker idx =
    if idx == 15 + 0 then
        Cell.Live

    else if idx == 15 + 1 then
        Cell.Live

    else if idx == 15 + 2 then
        Cell.Live

    else
        Cell.Deceased
