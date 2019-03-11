module Grid.Dimension exposing
    ( Dimension
    , getArea
    , make
    )


type alias Dimension =
    { h : Int -- height
    , w : Int -- width
    }


make : Int -> Int -> Dimension
make =
    Dimension


getArea : Dimension -> Int
getArea dim =
    dim.w * dim.h
