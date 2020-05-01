module World.GPS exposing (Direction(..), Location, advance, coordinates2D, decodeDirection, decodeLocation, encodeDirection, encodeLocation, location, toLeft, toRight)

import Json.Decode as Decode exposing (Decoder, int)
import Json.Decode.Pipeline exposing (required, resolve)
import Json.Encode as Encode


type Location
    = Location { x : Int, y : Int }


location : Int -> Int -> Location
location x y =
    Location { x = x, y = y }


coordinates2D : Location -> ( Int, Int )
coordinates2D (Location { x, y }) =
    ( x, y )


decodeLocation : Decoder Location
decodeLocation =
    Decode.succeed location
        |> required "x" Decode.int
        |> required "y" Decode.int


encodeLocation : Location -> Encode.Value
encodeLocation (Location { x, y }) =
    Encode.object
        [ ( "x", Encode.int x )
        , ( "y", Encode.int y )
        ]


type Direction
    = North
    | East
    | South
    | West


toLeft : Direction -> Direction
toLeft direction =
    case direction of
        North ->
            West

        East ->
            North

        South ->
            East

        West ->
            South


toRight : Direction -> Direction
toRight direction =
    case direction of
        North ->
            East

        East ->
            South

        South ->
            West

        West ->
            North


advance : Direction -> Location -> Location
advance direction (Location ({ x, y } as aLocation)) =
    case direction of
        North ->
            Location { aLocation | y = y + 1 }

        East ->
            Location { aLocation | x = x + 1 }

        South ->
            Location { aLocation | y = y - 1 }

        West ->
            Location { aLocation | x = x - 1 }


decodeDirection : Decoder Direction
decodeDirection =
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


encodeDirection : Direction -> Encode.Value
encodeDirection direction =
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
