module WorldTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import World exposing (Error(..), executeAll, world)
import World.GPS exposing (Direction(..), location)
import World.Maze exposing (Element(..), emptyMaze, insertElement, insertRectangle)
import World.Robot exposing (Instruction(..), robot)


suite : Test
suite =
    let
        defaultMaze =
            emptyMaze
                |> insertRectangle ( location -2 -2, location 2 2 ) Floor
    in
    describe "World"
        [ describe "no obstructions"
            [ test "circle back" <|
                \_ ->
                    let
                        expected =
                            location 0 0
                                |> robot North
                                |> world defaultMaze
                                |> Ok

                        result =
                            location 0 0
                                |> robot North
                                |> world defaultMaze
                                |> executeAll [ Forward, Left, Forward, Left, Forward, Left, Forward, Left ]
                    in
                    Expect.equal expected result
            ]
        , describe "with obstructions"
            [ test "single floor tile" <|
                \_ ->
                    let
                        expected =
                            FellInAPit 0 (location 0 1)
                                |> Err

                        maze =
                            emptyMaze
                                |> insertElement (location 0 0) Floor

                        result =
                            location 0 0
                                |> robot North
                                |> world maze
                                |> executeAll [ Forward ]
                    in
                    Expect.equal expected result
            , test "single floor with a wall" <|
                \_ ->
                    let
                        expected =
                            HitAWall 0 (location 0 1)
                                |> Err

                        maze =
                            emptyMaze
                                |> insertElement (location 0 0) Floor
                                |> insertElement (location 0 1) Wall

                        result =
                            location 0 0
                                |> robot North
                                |> world maze
                                |> executeAll [ Forward ]
                    in
                    Expect.equal expected result
            ]
        ]
