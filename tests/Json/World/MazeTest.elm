module Json.World.MazeTest exposing (mazeFuzzer, suite)

import Expect
import Fuzz exposing (Fuzzer, constant, list, oneOf)
import Json.Decode as Decode
import Json.World.GPS.LocationTest exposing (locationFuzzer)
import Json.World.Maze as Maze
import Test exposing (Test, describe, fuzz, test)
import World.GPS.Location exposing (Location, location)
import World.Maze exposing (Maze, emptyMaze, insertTile)
import World.Maze.Tile exposing (Tile(..))


suite : Test
suite =
    describe "Maze"
        [ describe "decode"
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
            , fuzz mazeFuzzer "encode decode are inverses" <|
                \aMaze ->
                    let
                        expected =
                            aMaze
                                |> Ok

                        actual =
                            aMaze
                                |> Maze.encode
                                |> Decode.decodeValue Maze.decode
                    in
                    Expect.equal expected actual
            ]
        ]


mazeFuzzer : Fuzzer Maze
mazeFuzzer =
    let
        insert ( aLocation, aDirection ) aMaze =
            insertTile aLocation aDirection aMaze

        intoMaze =
            List.foldl insert emptyMaze
    in
    Fuzz.map intoMaze (list positionFuzzer)


positionFuzzer : Fuzzer ( Location, Tile )
positionFuzzer =
    Fuzz.map2 (\l d -> ( l, d )) locationFuzzer tileFuzzer


tileFuzzer : Fuzzer Tile
tileFuzzer =
    oneOf
        [ constant Floor
        , constant Pit
        , constant Wall
        , constant Target
        ]
