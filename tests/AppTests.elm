module AppTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main
import Test exposing (..)


suite : Test
suite =
    test "if it compiles, it works" <|
        \_ ->
            Expect.pass
