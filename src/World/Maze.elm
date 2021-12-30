module World.Maze exposing (Maze, boundingBox, emptyMaze, insertRectangle, insertTile, map, tileAt, view)

import Dict exposing (Dict)
import Html.Styled exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Svg.Styled as Svg exposing (Svg)
import World.GPS.Location as Location exposing (Location)
import World.Maze.BoundingBox as BoundingBox exposing (BoundingBox)
import World.Maze.Tile as Tile exposing (Tile(..))


type Maze
    = Maze (Dict Int (Dict Int Tile))


emptyMaze : Maze
emptyMaze =
    Maze Dict.empty


map : Maze -> Dict Int (Dict Int Tile)
map (Maze d) =
    d


insertRectangle : BoundingBox -> Tile -> Maze -> Maze
insertRectangle bbox element aMaze =
    BoundingBox.allLocations bbox
        |> List.foldl (swap insertTile <| element) aMaze


swap : (a -> b -> c) -> b -> a -> c
swap f b a =
    f a b


insertTile : Location -> Tile -> Maze -> Maze
insertTile location tile (Maze locations) =
    let
        ( x, y ) =
            Location.coordinates2D location

        insert : Maybe (Dict Int Tile) -> Maybe (Dict Int Tile)
        insert dictionary =
            dictionary
                |> Maybe.withDefault Dict.empty
                |> Dict.insert y tile
                |> Just
    in
    locations
        |> Dict.update x insert
        |> Maze


tileAt : Location -> Maze -> Tile
tileAt location (Maze dictionary) =
    let
        ( x, y ) =
            Location.coordinates2D location
    in
    dictionary
        |> Dict.get x
        |> Maybe.withDefault Dict.empty
        |> Dict.get y
        |> Maybe.withDefault Pit


view : Maze -> Svg msg
view aMaze =
    let
        toTile aLocation =
            ( aLocation, tileAt aLocation aMaze )

        tiles =
            aMaze
                |> boundingBox
                |> Maybe.map BoundingBox.allLocations
                |> Maybe.withDefault []
                |> List.map toTile
                |> List.map Tile.view
    in
    Svg.g [] tiles


boundingBox : Maze -> Maybe BoundingBox
boundingBox (Maze dictionary) =
    let
        switch ( y, x ) =
            ( x, y )

        pairUp ( x, ys ) =
            ys
                |> Dict.map (\_ _ -> x)
                |> Dict.toList
                |> List.map switch

        pairs =
            dictionary
                |> Dict.toList
                |> List.concatMap pairUp

        extremize extremizer ( x, y ) ( ex, ey ) =
            ( extremizer x ex, extremizer y ey )

        gather p =
            ( List.foldl (extremize min) p pairs
            , List.foldl (extremize max) p pairs
            )

        toBoundingBox ( ( llx, lly ), ( urx, ury ) ) =
            BoundingBox.bbox (Location.location llx lly) (Location.location urx ury)
    in
    pairs
        |> List.head
        |> Maybe.map gather
        |> Maybe.map toBoundingBox
