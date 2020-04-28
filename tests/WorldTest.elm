module WorldTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import World exposing (..)


suite : Test
suite =
    describe "World"
        [ describe "executing instructions"
            [ describe "forward"
                [ test "facing North" <|
                    \_ ->
                        let
                            expected =
                                location 0 1
                                    |> robot North
                                    |> world
                                    |> Ok

                            result =
                                location 0 0
                                    |> robot North
                                    |> world
                                    |> executeAll [ Forward ]
                        in
                        Expect.equal expected result
                 , test "facing East" <|
                    \_ ->
                        let
                            expected =
                                location 1 0
                                    |> robot East
                                    |> world
                                    |> Ok

                            result =
                                location 0 0
                                    |> robot East
                                    |> world
                                    |> executeAll [ Forward ]
                        in
                        Expect.equal expected result
                  , test "facing South" <|
                    \_ ->
                        let
                            expected =
                                location 0 -1
                                    |> robot South
                                    |> world
                                    |> Ok

                            result =
                                location 0 0
                                    |> robot South
                                    |> world
                                    |> executeAll [ Forward ]
                        in
                        Expect.equal expected result
                  , test "facing West" <|
                    \_ ->
                        let
                            expected =
                                location -1 0
                                    |> robot West
                                    |> world
                                    |> Ok

                            result =
                                location 0 0
                                    |> robot West
                                    |> world
                                    |> executeAll [ Forward ]
                        in
                        Expect.equal expected result
                 ]
            ]
        ]
