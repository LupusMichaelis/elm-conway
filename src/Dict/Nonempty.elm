module Dict.Nonempty exposing
    ( Nonempty(..)
    , diff
    , filter
    , foldl
    , foldr
    , fromDict
    , fromList
    , fromNonemptyList
    , get
    , insert
    , intersect
    , keys
    , map
    , member
    , merge
    , partition
    , remove
    , singleton
    , size
    , toList
    , union
    , update
    , values
    )

import Basic
import Dict exposing (Dict)
import List.Nonempty


type Nonempty comparable v
    = Nonempty comparable v (Dict comparable v)


make :
    ( comparable, v )
    -> ( comparable, v )
    -> (Dict comparable v -> Nonempty comparable v)
make ( k1, v1 ) ( k2, v2 ) =
    Nonempty
        (min k1 k2)
        (if k1 < k2 then
            v1

         else
            v2
        )


fromDict : Dict comparable v -> Maybe (Nonempty comparable v)
fromDict dict =
    if 0 == Dict.size dict then
        Nothing

    else
        let
            mk =
                dict
                    |> Dict.keys
                    |> List.minimum

            mv =
                mk
                    |> Maybe.andThen (Basic.flip Dict.get dict)
        in
        Tuple.pair mk mv
            |> Basic.uncurry
                (Maybe.map2
                    (\k v -> Nonempty k v dict)
                )


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
    Nonempty key v <| Dict.singleton key v


insert : comparable -> v -> Nonempty comparable v -> Nonempty comparable v
insert k v (Nonempty bk bv d) =
    make
        ( k, v )
        ( bk, bv )
        (Dict.insert k v d)


update :
    comparable
    -> (Maybe v -> Maybe v)
    -> Nonempty comparable v
    -> Nonempty comparable v
update c fn (Nonempty k v d) =
    Nonempty k v <| Dict.update c fn d


remove :
    comparable
    -> Nonempty comparable v
    -> Maybe (Nonempty comparable v)
remove k (Nonempty _ _ d) =
    d |> Dict.remove k |> fromDict


member : comparable -> Nonempty comparable v -> Bool
member k (Nonempty bk _ d) =
    k == bk || Dict.member k d


get : comparable -> Nonempty comparable v -> Maybe v
get c (Nonempty _ _ d) =
    Dict.get c d


size : Nonempty comparable v -> Int
size (Nonempty _ _ d) =
    Dict.size d


keys : Nonempty comparable v -> List comparable
keys (Nonempty _ _ d) =
    Dict.keys d


values : Nonempty comparable v -> List v
values (Nonempty _ _ d) =
    Dict.values d


toList : Nonempty comparable v -> List ( comparable, v )
toList (Nonempty _ _ d) =
    Dict.toList d


map : (comparable -> a -> b) -> Nonempty comparable a -> Nonempty comparable b
map fn (Nonempty k v d) =
    Nonempty k (fn k v) (Dict.map fn d)


foldl : (comparable -> v -> b -> b) -> b -> Nonempty comparable v -> b
foldl fn v (Nonempty _ _ d) =
    Dict.foldl fn v d


foldr : (comparable -> v -> b -> b) -> b -> Nonempty comparable v -> b
foldr fn v (Nonempty _ _ d) =
    Dict.foldr fn v d


filter :
    (comparable -> v -> Bool)
    -> Nonempty comparable v
    -> Maybe (Nonempty comparable v)
filter fn (Nonempty _ _ d) =
    Dict.filter fn d
        |> fromDict


partition :
    (comparable -> v -> Bool)
    -> Nonempty comparable v
    -> ( Maybe (Nonempty comparable v), Maybe (Nonempty comparable v) )
partition fn (Nonempty _ _ d) =
    Dict.partition fn d
        |> Tuple.mapFirst fromDict
        |> Tuple.mapSecond fromDict


union :
    Nonempty comparable v
    -> Nonempty comparable v
    -> Nonempty comparable v
union (Nonempty k1 v1 d1) (Nonempty k2 v2 d2) =
    make
        ( k1, v1 )
        ( k2, v2 )
        (Dict.union d1 d2)


intersect :
    Nonempty comparable v
    -> Nonempty comparable v
    -> Maybe (Nonempty comparable v)
intersect (Nonempty _ _ d1) (Nonempty _ _ d2) =
    fromDict <| Dict.intersect d1 d2


diff :
    Nonempty comparable v
    -> Nonempty comparable v
    -> Maybe (Nonempty comparable v)
diff (Nonempty _ _ d1) (Nonempty _ _ d2) =
    fromDict <| Dict.diff d1 d2


merge :
    (comparable -> a -> result -> result)
    -> (comparable -> a -> b -> result -> result)
    -> (comparable -> b -> result -> result)
    -> Nonempty comparable a
    -> Nonempty comparable b
    -> result
    -> result
merge f1 f2 f3 (Nonempty _ _ nd1) (Nonempty _ _ nd2) =
    Dict.merge f1 f2 f3 nd1 nd2
