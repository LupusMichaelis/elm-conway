module Basic exposing
    ( curry
    , flip
    , swap
    , uncurry
    )


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
