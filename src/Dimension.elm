module Dimension exposing
    ( Two
    , getArea
    , make
    )


type alias Two =
    { h : Int -- height
    , w : Int -- width
    }


make : Int -> Int -> Two
make =
    Two


getArea : Two -> Int
getArea dim =
    dim.w * dim.h
