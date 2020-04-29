module RobotTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import World.GPS exposing (Direction(..), location)
import World.Robot exposing (Instruction(..), interpret, robot)


suite : Test
suite =
    describe "Robot"
        [ describe "execute"
            [ describe "forward"
                [ test "facing North" <|
                    \_ ->
                        let
                            expected =
                                location 0 1
                                    |> robot North

                            result =
                                location 0 0
                                    |> robot North
                                    |> interpret Forward
                        in
                        Expect.equal expected result
                , test "facing East" <|
                    \_ ->
                        let
                            expected =
                                location 1 0
                                    |> robot East

                            result =
                                location 0 0
                                    |> robot East
                                    |> interpret Forward
                        in
                        Expect.equal expected result
                , test "facing South" <|
                    \_ ->
                        let
                            expected =
                                location 0 -1
                                    |> robot South

                            result =
                                location 0 0
                                    |> robot South
                                    |> interpret Forward
                        in
                        Expect.equal expected result
                , test "facing West" <|
                    \_ ->
                        let
                            expected =
                                location -1 0
                                    |> robot West

                            result =
                                location 0 0
                                    |> robot West
                                    |> interpret Forward
                        in
                        Expect.equal expected result
                , describe "right"
                    [ test "facing North" <|
                        \_ ->
                            let
                                expected =
                                    location 0 0
                                        |> robot East

                                result =
                                    location 0 0
                                        |> robot North
                                        |> interpret Right
                            in
                            Expect.equal expected result
                    , test "facing East" <|
                        \_ ->
                            let
                                expected =
                                    location 0 0
                                        |> robot South

                                result =
                                    location 0 0
                                        |> robot East
                                        |> interpret Right
                            in
                            Expect.equal expected result
                    , test "facing South" <|
                        \_ ->
                            let
                                expected =
                                    location 0 0
                                        |> robot West

                                result =
                                    location 0 0
                                        |> robot South
                                        |> interpret Right
                            in
                            Expect.equal expected result
                    , test "facing West" <|
                        \_ ->
                            let
                                expected =
                                    location 0 0
                                        |> robot North

                                result =
                                    location 0 0
                                        |> robot West
                                        |> interpret Right
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

                                result =
                                    location 0 0
                                        |> robot North
                                        |> interpret Left
                            in
                            Expect.equal expected result
                    , test "facing East" <|
                        \_ ->
                            let
                                expected =
                                    location 0 0
                                        |> robot North

                                result =
                                    location 0 0
                                        |> robot East
                                        |> interpret Left
                            in
                            Expect.equal expected result
                    , test "facing South" <|
                        \_ ->
                            let
                                expected =
                                    location 0 0
                                        |> robot East

                                result =
                                    location 0 0
                                        |> robot South
                                        |> interpret Left
                            in
                            Expect.equal expected result
                    , test "facing West" <|
                        \_ ->
                            let
                                expected =
                                    location 0 0
                                        |> robot South

                                result =
                                    location 0 0
                                        |> robot West
                                        |> interpret Left
                            in
                            Expect.equal expected result
                    ]
                ]
            ]
        ]
