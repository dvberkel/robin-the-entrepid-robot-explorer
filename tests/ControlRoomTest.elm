module ControlRoomTest exposing (suite, levelFuzzer)

import ControlRoom exposing (level)
import Expect
import Fuzz exposing (Fuzzer, int)
import Json.Decode as Decode
import Test exposing (Test, describe, fuzz, test)
import ControlRoom exposing (Level, level)
import World exposing (world)
import World.GPS exposing (Direction(..), location)
import World.Maze exposing (Tile(..), emptyMaze, insertTile)
import World.Robot exposing (Instruction(..), robot)
import WorldTest exposing (worldFuzzer)


suite : Test
suite =
    describe "ControlRoom"
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
                                |> Decode.decodeString ControlRoom.decode
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
                                |> ControlRoom.encode
                                |> Decode.decodeValue ControlRoom.decode
                    in
                    Expect.equal expected actual
            ]
        ]


levelFuzzer : Fuzzer Level
levelFuzzer =
    Fuzz.map2 level int worldFuzzer
