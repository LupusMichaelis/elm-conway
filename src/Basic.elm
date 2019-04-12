module Basic exposing
    ( flip
    )

flip : (b -> a -> c) -> a -> b -> c
flip f a b = f b a
