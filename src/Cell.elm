module Cell exposing
    ( Rule(..)
    , State(..)
    , abideByTheRule
    , b36s23
    , b3s23
    , fateOf
    )

import Array exposing (Array)
import Basic


type State
    = Live -- happy cell shanting around
    | Deceased -- a corpse's lying there


b3s23 : List (Rule State)
b3s23 =
    [ Match [ 2, 3 ] Live Live Live
    , Match [ 3 ] Live Deceased Live
    , Always Deceased
    ]


b36s23 : List (Rule State)
b36s23 =
    [ Match [ 3, 6 ] Live Deceased Live
    , Match [ 2, 3 ] Live Live Deceased
    , Always Deceased
    ]



-- Match [count] neighbourState currentState newState


type Rule state
    = Match (List Int) state state state
    | Always state


fateOf :
    List (Rule state)
    -> state
    -> Array state
    -> state
fateOf rules cellState neighbourStateList =
    case rules of
        rule :: remains ->
            if ruleApplies rule cellState neighbourStateList then
                abideByTheRule rule cellState neighbourStateList

            else
                fateOf remains cellState neighbourStateList

        [] ->
            cellState


ruleApplies :
    Rule state
    -> state
    -> Array state
    -> Bool
ruleApplies rule cellState neighbourStateList =
    case rule of
        Match neighbourCount neighbourState fromState toState ->
            fromState
                == cellState
                && (neighbourStateList
                        |> Array.filter ((==) neighbourState)
                        |> Array.length
                        |> Basic.flip List.member neighbourCount
                   )

        Always _ ->
            True


abideByTheRule :
    Rule state
    -> state
    -> Array state
    -> state
abideByTheRule rule cellState neighbourStateList =
    case rule of
        Match neighbourCount neighbourState fromState toState ->
            if
                neighbourStateList
                    |> Array.filter ((==) neighbourState)
                    |> Array.length
                    |> Basic.flip List.member neighbourCount
            then
                toState

            else
                cellState

        Always toState ->
            toState
