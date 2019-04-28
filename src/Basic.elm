module Basic exposing
    ( compose2
    , curry
    , flip
    , swap
    , uncurry
    )


compose2 : (a -> b -> c) -> (c -> d) -> a -> b -> d
compose2 f next a b =
    f a b |> next


flip : (b -> a -> c) -> a -> b -> c
flip f a b =
    f b a


curry : (( a, b ) -> c) -> a -> b -> c
curry f a b =
    f ( a, b )


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )
