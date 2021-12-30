module Json.World.Maze.Tile exposing (decode, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import World.GPS.Direction exposing (Direction(..))
import World.Maze.Tile exposing (Tile(..))


encode : Tile -> Encode.Value
encode tile =
    let
        value =
            case tile of
                Floor ->
                    "Floor"

                Pit ->
                    "Pit"

                Wall ->
                    "Wall"

                Target ->
                    "Target"
    in
    Encode.string value


decode : Decoder Tile
decode =
    let
        toTile input =
            case input of
                "Floor" ->
                    Decode.succeed Floor

                "Pit" ->
                    Decode.succeed Pit

                "Wall" ->
                    Decode.succeed Wall

                "Target" ->
                    Decode.succeed Target

                _ ->
                    Decode.fail <| "'" ++ input ++ "' is not a tile"
    in
    Decode.string
        |> Decode.andThen toTile
