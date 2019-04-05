module Grid.Cell exposing
    ( State(..)
    , fateOf
    , shouldACellDie
    , shouldACellResurrect
    )


type State
    = Live -- happy cell shanting around
    | Deceased -- a corpse's lying there


shouldACellDie : List State -> Bool
shouldACellDie neighbours =
    let
        living =
            List.filter (\state -> Live == state) neighbours
                |> List.length
    in
    living < 2 || living > 3


shouldACellResurrect : List State -> Bool
shouldACellResurrect neighbours =
    let
        living =
            List.filter (\state -> Live == state) neighbours
                |> List.length
    in
    living == 3


fateOf : State -> List State -> State
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
