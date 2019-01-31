module Grid.Cell exposing
    ( State(..)

    , fateOf
    , shouldACellBirth
    , shouldACellDie
    , shouldACellResurrect
    )

import Array exposing (Array)

type State =
    Live            -- happy cell shanting around
    | Deceased      -- a corpse's lying there
    | Empty         -- no live cell's present

shouldACellBirth: Array State -> Bool
shouldACellBirth =
    shouldACellResurrect

shouldACellDie: Array State -> Bool
shouldACellDie neighbours =
    let living =
        Array.filter (\ state -> Live == state) neighbours
            |> Array.length
    in
        living < 2 || living > 3

shouldACellResurrect: Array State -> Bool
shouldACellResurrect neighbours =
    let living =
        Array.filter (\ state -> Live == state ) neighbours
            |> Array.length
    in
        living == 3

fateOf: State -> Array State -> State
fateOf cellState neighbourStates =
    case cellState of
        Live ->
            case shouldACellDie neighbourStates of
                True ->
                    Deceased
                False ->
                    cellState
        Deceased ->
            case shouldACellResurrect neighbourStates of
                True ->
                    Live
                False ->
                    cellState
        Empty ->
            case shouldACellBirth neighbourStates of
                True ->
                    Live
                False ->
                    cellState
