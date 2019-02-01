module Grid.Cell exposing
    ( State(..)

    , fateOf
    , shouldACellDie
    , shouldACellResurrect
    )

import Array exposing (Array)

type State =
    Live            -- happy cell shanting around
    | Deceased      -- a corpse's lying there

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
