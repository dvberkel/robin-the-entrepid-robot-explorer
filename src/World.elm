module World exposing (Error(..), World, decode, encode, executeAll, view, world)

import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Svg
import Svg.Attributes as Attribute
import World.GPS as GPS exposing (Location)
import World.Maze as Maze exposing (Maze, Tile(..), tileAt)
import World.Robot as Robot exposing (Instruction, Robot)


type World
    = World
        { robot : Robot
        , maze : Maze
        }


world : Maze -> Robot -> World
world aMaze aRobot =
    World
        { robot = aRobot
        , maze = aMaze
        }


executeAll : List Instruction -> World -> Result Error World
executeAll instructions start =
    let
        enumerate index instruction =
            ( index, instruction )

        actOn : ( Int, Instruction ) -> Result Error World -> Result Error World
        actOn instruction aWorld =
            Result.andThen (execute instruction) aWorld
    in
    instructions
        |> List.indexedMap enumerate
        |> List.foldl actOn (Ok start)


execute : ( Int, Instruction ) -> World -> Result Error World
execute ( instructionPointer, instruction ) (World aWorld) =
    let
        intention =
            Robot.execute instruction aWorld.robot

        targetLocation =
            Robot.location intention

        targetTile =
            tileAt targetLocation aWorld.maze
    in
    case targetTile of
        Floor ->
            Ok <| World { aWorld | robot = intention }

        Wall ->
            Err <| HitAWall instructionPointer targetLocation

        Pit ->
            Err <| FellInAPit instructionPointer targetLocation

        Target ->
            Ok <| World { aWorld | robot = intention }



-- TODO mark victory


type Error
    = HitAWall Int Location
    | FellInAPit Int Location


encode : World -> Encode.Value
encode (World aWorld) =
    Encode.object
        [ ( "robot", Robot.encode aWorld.robot )
        , ( "maze", Maze.encode aWorld.maze )
        ]


decode : Decoder World
decode =
    Decode.succeed world
        |> required "maze" Maze.decode
        |> required "robot" Robot.decode


view : World -> Html msg
view (World aWorld) =
    let
        ( ll, ur ) =
            aWorld.maze
                |> Maze.boundingBox
                |> Maybe.withDefault ( GPS.location 0 0, GPS.location 1 1 )

        ( minX, minY ) =
            GPS.coordinates2D ll

        ( maxX, maxY ) =
            GPS.coordinates2D ur
                |> (\( x, y ) -> ( x + 1, y + 1 ))

        side =
            max (maxX - minX) (maxY - minY)

        viewBox =
            [ minX, minY, side, side ]
                |> List.map String.fromInt
                |> String.join " "
    in
    Svg.svg
        [ Attribute.width "640"
        , Attribute.height "640"
        , Attribute.viewBox viewBox
        ]
        [ Svg.rect
            [ Attribute.x <| String.fromInt minX
            , Attribute.y <| String.fromInt minY
            , Attribute.width <| String.fromInt side
            , Attribute.height <| String.fromInt side
            , Attribute.fill "black"
            ]
            []
        , Maze.view aWorld.maze
        , Robot.view aWorld.robot
        ]
