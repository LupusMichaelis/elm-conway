module Cell exposing
    ( State(..)
    , fateOf
    , shouldACellDie
    , shouldACellResurrect
    )

import Basic


type State
    = Live -- happy cell shanting around
    | Deceased -- a corpse's lying there


shouldACellDie : List State -> Bool
shouldACellDie neighbours =
    List.filter ((==) Live) neighbours
        |> List.length
        |> Basic.flip List.member [ 2, 3 ]
        |> not


shouldACellResurrect : List State -> Bool
shouldACellResurrect neighbours =
    List.filter ((==) Live) neighbours
        |> List.length
        |> (==) 3


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
