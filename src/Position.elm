module Position exposing
    ( Two
    , fromTuple
    , make
    , toTuple
    )


type alias Two =
    { t : Int -- from top
    , l : Int -- from left
    }


make : Int -> Int -> Two
make =
    Two


toTuple : Two -> ( Int, Int )
toTuple { t, l } =
    ( t, l )


fromTuple : ( Int, Int ) -> Two
fromTuple ( t, l ) =
    Two t l
