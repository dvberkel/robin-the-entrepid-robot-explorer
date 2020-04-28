module WorldTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import World exposing (..)


suite : Test
suite =
    describe "World"
        [ describe "executing instructions"
            [ describe "single instruction"
                [ describe "forward"
                    [ test "facing North" <|
                        \_ ->
                            let
                                expected =
                                    location 0 1
                                        |> robot North
                                        |> world emptyMaze
                                        |> Ok

                                result =
                                    location 0 0
                                        |> robot North
                                        |> world emptyMaze
                                        |> executeAll [ Forward ]
                            in
                            Expect.equal expected result
                    , test "facing East" <|
                        \_ ->
                            let
                                expected =
                                    location 1 0
                                        |> robot East
                                        |> world emptyMaze
                                        |> Ok

                                result =
                                    location 0 0
                                        |> robot East
                                        |> world emptyMaze
                                        |> executeAll [ Forward ]
                            in
                            Expect.equal expected result
                    , test "facing South" <|
                        \_ ->
                            let
                                expected =
                                    location 0 -1
                                        |> robot South
                                        |> world emptyMaze
                                        |> Ok

                                result =
                                    location 0 0
                                        |> robot South
                                        |> world emptyMaze
                                        |> executeAll [ Forward ]
                            in
                            Expect.equal expected result
                    , test "facing West" <|
                        \_ ->
                            let
                                expected =
                                    location -1 0
                                        |> robot West
                                        |> world emptyMaze
                                        |> Ok

                                result =
                                    location 0 0
                                        |> robot West
                                        |> world emptyMaze
                                        |> executeAll [ Forward ]
                            in
                            Expect.equal expected result
                    , describe "right"
                        [ test "facing North" <|
                            \_ ->
                                let
                                    expected =
                                        location 0 0
                                            |> robot East
                                            |> world emptyMaze
                                            |> Ok

                                    result =
                                        location 0 0
                                            |> robot North
                                            |> world emptyMaze
                                            |> executeAll [ Right ]
                                in
                                Expect.equal expected result
                        , test "facing East" <|
                            \_ ->
                                let
                                    expected =
                                        location 0 0
                                            |> robot South
                                            |> world emptyMaze
                                            |> Ok

                                    result =
                                        location 0 0
                                            |> robot East
                                            |> world emptyMaze
                                            |> executeAll [ Right ]
                                in
                                Expect.equal expected result
                        , test "facing South" <|
                            \_ ->
                                let
                                    expected =
                                        location 0 0
                                            |> robot West
                                            |> world emptyMaze
                                            |> Ok

                                    result =
                                        location 0 0
                                            |> robot South
                                            |> world emptyMaze
                                            |> executeAll [ Right ]
                                in
                                Expect.equal expected result
                        , test "facing West" <|
                            \_ ->
                                let
                                    expected =
                                        location 0 0
                                            |> robot North
                                            |> world emptyMaze
                                            |> Ok

                                    result =
                                        location 0 0
                                            |> robot West
                                            |> world emptyMaze
                                            |> executeAll [ Right ]
                                in
                                Expect.equal expected result
                        ]
                    , describe "left"
                        [ test "facing North" <|
                            \_ ->
                                let
                                    expected =
                                        location 0 0
                                            |> robot West
                                            |> world emptyMaze
                                            |> Ok

                                    result =
                                        location 0 0
                                            |> robot North
                                            |> world emptyMaze
                                            |> executeAll [ Left ]
                                in
                                Expect.equal expected result
                        , test "facing East" <|
                            \_ ->
                                let
                                    expected =
                                        location 0 0
                                            |> robot North
                                            |> world emptyMaze
                                            |> Ok

                                    result =
                                        location 0 0
                                            |> robot East
                                            |> world emptyMaze
                                            |> executeAll [ Left ]
                                in
                                Expect.equal expected result
                        , test "facing South" <|
                            \_ ->
                                let
                                    expected =
                                        location 0 0
                                            |> robot East
                                            |> world emptyMaze
                                            |> Ok

                                    result =
                                        location 0 0
                                            |> robot South
                                            |> world emptyMaze
                                            |> executeAll [ Left ]
                                in
                                Expect.equal expected result
                        , test "facing West" <|
                            \_ ->
                                let
                                    expected =
                                        location 0 0
                                            |> robot South
                                            |> world emptyMaze
                                            |> Ok

                                    result =
                                        location 0 0
                                            |> robot West
                                            |> world emptyMaze
                                            |> executeAll [ Left ]
                                in
                                Expect.equal expected result
                        ]
                    ]
                ]
            ]
        , describe "multiple instructions"
            [ test "circle back" <|
                \_ ->
                    let
                        expected =
                            location 0 0
                                |> robot North
                                |> world emptyMaze
                                |> Ok

                        result =
                            location 0 0
                                |> robot North
                                |> world emptyMaze
                                |> executeAll [ Forward, Left, Forward, Left, Forward, Left, Forward, Left ]
                    in
                    Expect.equal expected result
            ]
        ]
