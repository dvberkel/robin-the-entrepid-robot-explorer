module WorldTest exposing (suite, worldFuzzer)

import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as Decode
import Test exposing (Test, describe, fuzz, test)
import World exposing (Error(..), World, executeAll, world)
import World.GPS.Direction exposing (Direction(..))
import World.GPS.Location exposing (location)
import World.Json as Json
import World.Json.MazeTest exposing (mazeFuzzer)
import World.Json.RobotTest exposing (robotFuzzer)
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
        , describe "decode"
            [ test "of a world" <|
                \_ ->
                    let
                        aRobot =
                            location 0 0
                                |> robot North

                        aMaze =
                            emptyMaze
                                |> insertTile (location 0 0) Floor
                                |> insertTile (location 0 1) Wall

                        expected =
                            world aMaze aRobot
                                |> Ok

                        actual =
                            """{"robot": {"location": {"x": 0, "y": 0}, "direction": "North"}, "maze": {"0,0": "Floor", "0,1": "Wall"}}"""
                                |> Decode.decodeString Json.decode
                    in
                    Expect.equal expected actual
            , fuzz worldFuzzer "encode decode are inverses" <|
                \aWorld ->
                    let
                        expected =
                            aWorld
                                |> Ok

                        actual =
                            aWorld
                                |> Json.encode
                                |> Decode.decodeValue Json.decode
                    in
                    Expect.equal expected actual
            ]
        ]


worldFuzzer : Fuzzer World
worldFuzzer =
    Fuzz.map2 world mazeFuzzer robotFuzzer
