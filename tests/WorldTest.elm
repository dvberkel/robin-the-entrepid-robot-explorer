module WorldTest exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, test)
import World exposing (Error(..), executeAll, world)
import World.GPS.Direction exposing (Direction(..))
import World.GPS.Location exposing (location)
import World.Maze exposing (emptyMaze, insertRectangle, insertTile)
import World.Maze.BoundingBox exposing (bbox)
import World.Maze.Tile exposing (Tile(..))
import World.Robot exposing (robot)
import World.Robot.Instruction exposing (Instruction(..))


suite : Test
suite =
    let
        bb =
            bbox (location -2 -2) (location 2 2)

        defaultMaze =
            emptyMaze
                |> insertRectangle bb Floor
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
                                |> insertTile (location 0 0) Floor

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
                                |> insertTile (location 0 0) Floor
                                |> insertTile (location 0 1) Wall

                        result =
                            location 0 0
                                |> robot North
                                |> world maze
                                |> executeAll [ Forward ]
                    in
                    Expect.equal expected result
            ]
        ]
