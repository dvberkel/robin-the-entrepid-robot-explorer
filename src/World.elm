module World exposing (Error(..), World, executeAll, maze, reset, robot, view, world)

import Html.Styled exposing (Html)
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attribute
import World.GPS.Location as Location exposing (Location)
import World.Maze as Maze exposing (Maze, tileAt)
import World.Maze.BoundingBox as BoundingBox
import World.Maze.Tile exposing (Tile(..))
import World.Robot as Robot exposing (Robot)
import World.Robot.Instruction exposing (Instruction)


type World
    = World
        { robot : Robot
        , initialRobot : Robot
        , maze : Maze
        }


world : Maze -> Robot -> World
world aMaze aRobot =
    World
        { robot = aRobot
        , initialRobot = aRobot
        , maze = aMaze
        }


robot : World -> Robot
robot (World w) =
    w.robot


maze : World -> Maze
maze (World w) =
    w.maze


reset : World -> World
reset (World aWorld) =
    World { aWorld | robot = aWorld.initialRobot }


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


view : World -> Html msg
view (World aWorld) =
    let
        bbox =
            aWorld.maze
                |> Maze.boundingBox
                |> Maybe.withDefault BoundingBox.default

        ( minX, minY ) =
            bbox
                |> BoundingBox.lowerLeft
                |> Location.coordinates2D

        ( maxX, maxY ) =
            bbox
                |> BoundingBox.upperRight
                |> Location.coordinates2D
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
