module Json.World.Maze exposing (decode, encode)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Json.World.Maze.Tile as Tile
import World.GPS.Direction exposing (Direction(..))
import World.GPS.Location as Location
import World.Maze as Maze exposing (Maze)
import World.Maze.Tile exposing (Tile)


encode : Maze -> Encode.Value
encode maze =
    let
        dictionary =
            Maze.map maze

        toYTiles : Int -> Dict Int Tile -> List ( Int, Tile )
        toYTiles _ =
            Dict.toList

        toXYTiles : ( Int, List ( Int, Tile ) ) -> List ( ( Int, Int ), Tile )
        toXYTiles ( x, ys ) =
            List.map (\( y, tile ) -> ( ( x, y ), tile )) ys

        toObjectPairs ( ( x, y ), tile ) =
            ( String.join "," <| List.map String.fromInt [ x, y ], Tile.encode tile )
    in
    dictionary
        |> Dict.map toYTiles
        |> Dict.toList
        |> List.concatMap toXYTiles
        |> List.map toObjectPairs
        |> Encode.object


decode : Decoder Maze
decode =
    let
        parse ( input, tile ) =
            input
                |> Location.parse
                |> Result.map (\location -> ( location, tile ))

        insert ( location, aTile ) aMaze =
            Maze.insertTile location aTile aMaze

        toMaze xs =
            xs
                |> List.foldl insert Maze.emptyMaze
    in
    Decode.keyValuePairs Tile.decode
        |> parseMap (\( input, _ ) -> input) parse
        |> Decode.map toMaze


parseMap : (a -> String) -> (a -> Result e b) -> Decoder (List a) -> Decoder (List b)
parseMap toString parse xss =
    let
        decoder xs =
            case xs of
                [] ->
                    Decode.succeed []

                ( index, h ) :: tail ->
                    case parse h of
                        Ok y ->
                            decoder tail
                                |> Decode.map (\ys -> ( index, y ) :: ys)

                        Err _ ->
                            Decode.fail <| "could not parse element '" ++ toString h ++ "' at index: " ++ String.fromInt index
    in
    xss
        |> Decode.map enumerate
        |> Decode.andThen decoder
        |> Decode.map denumerate


enumerate : List a -> List ( Int, a )
enumerate =
    indexedEnumerate 0 []


denumerate : List ( Int, a ) -> List a
denumerate =
    tailRecursiveDenumerate []


tailRecursiveDenumerate : List a -> List ( Int, a ) -> List a
tailRecursiveDenumerate accumulator xs =
    case xs of
        [] ->
            List.reverse accumulator

        ( _, h ) :: tail ->
            tailRecursiveDenumerate (h :: accumulator) tail


indexedEnumerate : Int -> List ( Int, a ) -> List a -> List ( Int, a )
indexedEnumerate index accumulator xs =
    case xs of
        [] ->
            List.reverse accumulator

        h :: tail ->
            indexedEnumerate (index + 1) (( index, h ) :: accumulator) tail
