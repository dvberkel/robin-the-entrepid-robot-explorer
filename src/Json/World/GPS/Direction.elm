module Json.World.GPS.Direction exposing (decode, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import World.GPS.Direction exposing (Direction(..))


decode : Decoder Direction
decode =
    let
        toDirection input =
            case input of
                "North" ->
                    Decode.succeed North

                "East" ->
                    Decode.succeed East

                "South" ->
                    Decode.succeed South

                "West" ->
                    Decode.succeed West

                _ ->
                    Decode.fail <| "'" ++ input ++ "' is not a direction"
    in
    Decode.string
        |> Decode.andThen toDirection


encode : Direction -> Encode.Value
encode direction =
    let
        value =
            case direction of
                North ->
                    "North"

                East ->
                    "East"

                South ->
                    "South"

                West ->
                    "West"
    in
    Encode.string value
