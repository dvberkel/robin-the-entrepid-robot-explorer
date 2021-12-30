module Json.World.Robot exposing (decode, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Json.World.GPS.Direction as Direction
import Json.World.GPS.Location as Location
import World.Robot as Robot exposing (Robot)


encode : Robot -> Encode.Value
encode robot =
    let
        location =
            Robot.location robot

        direction =
            Robot.direction robot
    in
    Encode.object
        [ ( "location", Location.encode location )
        , ( "direction", Direction.encode direction )
        ]


decode : Decoder Robot
decode =
    Decode.succeed Robot.robot
        |> required "direction" Direction.decode
        |> required "location" Location.decode
