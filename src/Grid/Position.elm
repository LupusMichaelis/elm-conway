module Grid.Position exposing
    ( Position
    , make
    )


type alias Position =
    { t : Int -- from top
    , l : Int -- from left
    }


make : Int -> Int -> Position
make =
    Position
