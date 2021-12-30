module Json.Control.LevelTest exposing (levelFuzzer, suite)

import Control.Level as Level exposing (Level, level)
import Expect
import Fuzz exposing (Fuzzer, int)
import Json.Decode as Decode
import Json.WorldTest exposing (worldFuzzer)
import Test exposing (Test, describe, fuzz, test)
import World exposing (world)
import World.GPS.Direction exposing (Direction(..))
import World.GPS.Location exposing (location)
import World.Maze exposing (emptyMaze, insertTile)
import World.Maze.Tile exposing (Tile(..))
import World.Robot exposing (robot)
import World.Robot.Instruction exposing (Instruction(..))


suite : Test
suite =
    describe "Json"
        [ describe "Level"
            [ describe "decode"
                [ test "of a level" <|
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
                                    |> level 0
                                    |> Ok

                            actual =
                                """{"index": 0, "world": {"robot": {"location": {"x": 0, "y": 0}, "direction": "North"}, "maze": {"0,0": "Floor", "0,1": "Wall"}}}"""
                                    |> Decode.decodeString Level.decode
                        in
                        Expect.equal expected actual
                , fuzz levelFuzzer "encode decode are inverses" <|
                    \aLevel ->
                        let
                            expected =
                                aLevel
                                    |> Ok

                            actual =
                                aLevel
                                    |> Level.encode
                                    |> Decode.decodeValue Level.decode
                        in
                        Expect.equal expected actual
                ]
            ]
        ]


levelFuzzer : Fuzzer Level
levelFuzzer =
    Fuzz.map2 level int worldFuzzer
