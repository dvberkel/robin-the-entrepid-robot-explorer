module World.Maze.Tile exposing (Tile(..), view)

import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes as Attribute
import World.GPS.Location as Location exposing (Location)


type Tile
    = Floor
    | Pit
    | Wall
    | Target


view : ( Location, Tile ) -> Svg msg
view ( aLocation, tile ) =
    let
        ( x, y ) =
            Location.coordinates2D aLocation
    in
    Svg.rect
        [ Attribute.fill <| fillColor tile
        , Attribute.width "1"
        , Attribute.height "1"
        , Attribute.rx "0.1"
        , Attribute.ry "0.1"
        , Attribute.x <| String.fromInt x
        , Attribute.y <| String.fromInt y
        ]
        []


fillColor : Tile -> String
fillColor tile =
    case tile of
        Pit ->
            "black"

        Floor ->
            "blue"

        Wall ->
            "red"

        Target ->
            "green"
