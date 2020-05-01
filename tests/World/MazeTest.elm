module World.MazeTest exposing (suite)

import Expect
import Fuzz exposing (Fuzzer, constant, int, oneOf)
import Json.Decode as Decode
import Test exposing (Test, describe, fuzz, test)
import World.GPS exposing (Location, location)
import World.Maze as Maze exposing (Tile(..), Maze, emptyMaze, insertTile)


suite : Test
suite =
    describe "Maze"
        [describe "decode"
            [ test "decode a maze" <|
                \_ ->
                    let
                        expected =
                            emptyMaze
                                |> insertTile (location 0 0) Floor 
                                |> insertTile (location 1 0) Wall
                                |> insertTile (location 0 1) Pit
                                |> Ok

                        actual =
                            """{"0,0": "Floor", "1,0": "Wall", "0,1": "Pit"}"""
                                |> Decode.decodeString Maze.decode
                    in
                    Expect.equal expected actual
--            , fuzz mazeFuzzer "encode decode are inverses" <|
--                \aMaze ->
--                    let
--                        expected =
--                            aMaze
--                                |> Ok
--
--                        actual =
--                            aMaze
--                                |> Maze.encode
--                                |> Decode.decodeValue Maze.decode
--                    in
--                    Expect.equal expected actual
            ]
        ]