module Position exposing
    ( Two
    , fromTuple
    , make
    )


type alias Two =
    { t : Int -- from top
    , l : Int -- from left
    }


make : Int -> Int -> Two
make =
    Two


fromTuple : ( Int, Int ) -> Two
fromTuple t =
    Two (Tuple.first t) (Tuple.second t)
