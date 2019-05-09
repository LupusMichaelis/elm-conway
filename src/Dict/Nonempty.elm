module Dict.Nonempty exposing
    ( Nonempty
    , fromDict
    , fromList
    , fromNonemptyList
    , size
    )

import Basic
import Dict exposing (Dict)
import List.Nonempty


type Nonempty comparable v
    = Nonempty comparable (Dict comparable v)


fromDict : Dict comparable v -> Maybe (Nonempty comparable v)
fromDict dict =
    if 0 == Dict.size dict then
        Nothing

    else
        dict
            |> Dict.keys
            |> List.minimum
            |> Maybe.map
                (Basic.flip Nonempty dict)


fromList : List ( comparable, v ) -> Maybe (Nonempty comparable v)
fromList =
    Dict.fromList >> fromDict


fromNonemptyList : List.Nonempty.Nonempty ( comparable, v ) -> Nonempty comparable v
fromNonemptyList (List.Nonempty.Nonempty (( headKey, headValue ) as head) tail) =
    head
        :: tail
        |> Dict.fromList
        |> fromDict
        |> Maybe.withDefault (singleton headKey headValue)


singleton : comparable -> v -> Nonempty comparable v
singleton key v =
    Nonempty key <| Dict.singleton key v


size : Nonempty comparable v -> Int
size (Nonempty k d) =
    Dict.size d
