module Json.World.GPS.Location exposing (decode, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import World.GPS.Location as Location exposing (Location, location)


decode : Decoder Location
decode =
    Decode.succeed location
        |> required "x" Decode.int
        |> required "y" Decode.int


encode : Location -> Encode.Value
encode loc =
    let
        ( x, y ) =
            Location.coordinates2D loc
    in
    Encode.object
        [ ( "x", Encode.int x )
        , ( "y", Encode.int y )
        ]
