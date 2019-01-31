module Seeder exposing
    ( allDeceased
    , allEmpty
    , allLive
    , battlefield
    , evenAreLive
    , getCatalog
    , oddAreLive

    , blinker
    )

import Grid.Cell as Cell

import Array exposing (Array)

getCatalog: Array (String, Int -> Cell.State)
getCatalog =
    [ ("All empty cells", allEmpty)
    , ("All live cells", allLive)
    , ("All deceased cells", allDeceased)
    , ("Odd cells are live, other empty", oddAreLive)
    , ("Even cells are live, other empty", evenAreLive)
    , ("A battlefield with a living cell every 3 cell, a deceased every 11, rest empty", battlefield)
    ]
        |> Array.fromList

allEmpty: Int -> Cell.State
allEmpty _ =
    Cell.Empty

allLive: Int -> Cell.State
allLive _ =
    Cell.Live

allDeceased: Int -> Cell.State
allDeceased _ =
    Cell.Deceased

oddAreLive: Int -> Cell.State
oddAreLive idx =
    if idx % 2 == 0 then
        Cell.Live
    else
        Cell.Empty

evenAreLive: Int -> Cell.State
evenAreLive idx =
    if idx % 2 == 0 then
        Cell.Empty
    else
        Cell.Live

battlefield: Int -> Cell.State
battlefield idx =
    if idx % 3 == 0 then
        Cell.Live
    else if idx % 11 == 0 then
        Cell.Deceased
    else
        Cell.Empty

blinker: Int -> Cell.State
blinker idx =
    if idx == 15 + 0 then
        Cell.Live
    else if idx == 15 + 1 then
        Cell.Live
    else if idx == 15 + 2 then
        Cell.Live
    else
        Cell.Empty
