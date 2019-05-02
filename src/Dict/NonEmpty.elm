module Dict.NonEmpty exposing
    ( NonEmpty
    , fromDict
    , fromList
    )

import Basic
import Dict exposing (Dict)


type NonEmpty comparable v
    = NonEmpty comparable (Dict comparable v)


fromDict : Dict comparable v -> Maybe (NonEmpty comparable v)
fromDict dict =
    if 0 == Dict.size dict then
        Nothing

    else
        let
            first : Maybe comparable
            first =
                dict |> Dict.keys |> List.minimum
        in
        first
            |> Maybe.map
                (Basic.flip NonEmpty dict)


fromList : List ( comparable, v ) -> Maybe (NonEmpty comparable v)
fromList =
    Dict.fromList >> fromDict
