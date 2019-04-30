module Cell exposing
    ( State(..)
    , fateOf
    , shouldACellDie
    , shouldACellResurrect
    )

import Array exposing (Array)
import Basic


type State
    = Live -- happy cell shanting around
    | Deceased -- a corpse's lying there


shouldACellDie : Array State -> Bool
shouldACellDie =
    Array.filter ((==) Live)
        >> Array.length
        >> Basic.flip List.member [ 2, 3 ]
        >> not


shouldACellResurrect : Array State -> Bool
shouldACellResurrect =
    Array.filter ((==) Live)
        >> Array.length
        >> (==) 3


fateOf : State -> Array State -> State
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
