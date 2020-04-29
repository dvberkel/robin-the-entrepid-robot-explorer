module WorldTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import World exposing (world, executeAll)
import World.GPS exposing (Direction(..), location)
import World.Maze exposing (Element(..), emptyMaze, insertRectangle)
import World.Robot exposing (Instruction(..), robot)


suite : Test
suite =
    let
        defaultMaze =
            emptyMaze
                |> insertRectangle ( location -2 -2, location 2 2 ) Tile
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
        ]
