module BasicTests exposing
    ( curryTests
    , swapTests
    , flipTests
    , uncurryTests
    )

import Basic
import Test exposing (..)
import Expect exposing (Expectation)

curryTests : Test
curryTests =
    describe "A function that get a second function and gives it two provided arguments as a tuple"
        [ test "Curry two numbers"
            (\_ ->
                Basic.curry identity 1 2
                    |> Expect.equal (1, 2)
            )
        ]

uncurryTests : Test
uncurryTests =
    describe "A function that get a second function and gives it two arguments containes in a tuple"
        [ test "Uncurry two numbers"
            (\_ ->
                Basic.uncurry (//) (6, 3)
                    |> Expect.equal 2
            )
        ]

flipTests : Test
flipTests =
    describe "Swap arguments of a function"
        [ test "Swap two numbers"
            (\_ ->
                Basic.flip Tuple.pair 6 3
                    |> Expect.equal (3, 6)
            )
        ]

swapTests : Test
swapTests =
    describe "Swap a tuple"
        [ test "Swap two numbers"
            (\_ ->
                Basic.swap (6, 3)
                    |> Expect.equal (3, 6)
            )
        ]
