module Seeder exposing
    ( Seeder

    , allDeceased
    , allEmpty
    , allLive
    , battlefield
    , evenAreLive
    , getCatalog
    , getDefaultSeeder
    , getDefaultSeederIndex
    , oddAreLive

    , blinker
    )

import Grid.Cell as Cell

import Array exposing (Array)

type alias Seeder = Int -> Cell.State

getCatalog: Array (String, Seeder)
getCatalog =
    [ ("All empty cells", allEmpty)
    , ("All live cells", allLive)
    , ("All deceased cells", allDeceased)
    , ("Odd cells are live, other empty", oddAreLive)
    , ("Even cells are live, other empty", evenAreLive)
    , ("A battlefield with a living cell every 3 cell, a deceased every 11, rest empty", battlefield)
    ]
        |> Array.fromList

-- TODO a better default
getDefaultSeeder: Seeder
getDefaultSeeder =
    allLive

-- TODO a better default
getDefaultSeederIndex: Int
getDefaultSeederIndex =
    1

allEmpty: Seeder
allEmpty _ =
    Cell.Empty

allLive: Seeder
allLive _ =
    Cell.Live

allDeceased: Seeder
allDeceased _ =
    Cell.Deceased

oddAreLive: Seeder
oddAreLive idx =
    if idx % 2 == 0 then
        Cell.Live
    else
        Cell.Empty

evenAreLive: Seeder
evenAreLive idx =
    if idx % 2 == 0 then
        Cell.Empty
    else
        Cell.Live

battlefield: Seeder
battlefield idx =
    if idx % 3 == 0 then
        Cell.Live
    else if idx % 11 == 0 then
        Cell.Deceased
    else
        Cell.Empty

blinker: Seeder
blinker idx =
    if idx == 15 + 0 then
        Cell.Live
    else if idx == 15 + 1 then
        Cell.Live
    else if idx == 15 + 2 then
        Cell.Live
    else
        Cell.Empty
