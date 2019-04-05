module Seeder exposing
    ( Seeder
    , allDeceased
    , allLive
    , battlefield
    , blinker
    , evenAreLive
    , getCatalog
    , getDefaultSeeder
    , oddAreLive
    )

import Grid.Cell as Cell


type alias Seeder =
    Int -> Cell.State


getCatalog : List ( String, Seeder )
getCatalog =
    [ ( "All live cells", allLive )
    , ( "All deceased cells", allDeceased )
    , ( "Odd cells are live, other empty", oddAreLive )
    , ( "Even cells are live, other empty", evenAreLive )
    , ( "A battlefield with a living cell every 3 and 5 cell", battlefield )
    ]


getDefaultSeeder : Seeder
getDefaultSeeder =
    allLive


allLive : Seeder
allLive _ =
    Cell.Live


allDeceased : Seeder
allDeceased _ =
    Cell.Deceased


oddAreLive : Seeder
oddAreLive idx =
    if idx % 2 == 0 then
        Cell.Live

    else
        Cell.Deceased


evenAreLive : Seeder
evenAreLive idx =
    if idx % 2 == 0 then
        Cell.Deceased

    else
        Cell.Live


battlefield : Seeder
battlefield idx =
    if
        idx
            % 3
            == 0
            || idx
            % 5
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
