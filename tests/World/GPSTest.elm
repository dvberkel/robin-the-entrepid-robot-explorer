module World.GPSTest exposing (directionFuzzer, locationFuzzer, suite)

import Expect
import Fuzz exposing (Fuzzer, constant, int, oneOf)
import Json.Decode as Decode
import Test exposing (Test, describe, fuzz, test)
import World.GPS as GPS exposing (Direction(..), Location, location)


suite : Test
suite =
    describe "GPS"
        [ describe "location"
            [ test "decode" <|
                \_ ->
                    let
                        expected =
                            location 0 0
                                |> Ok

                        actual =
                            """{"x": 0, "y": 0}"""
                                |> Decode.decodeString GPS.decodeLocation
                    in
                    Expect.equal expected actual
            , fuzz locationFuzzer "encode decode of location should be inverses" <|
                \aLocation ->
                    let
                        expected =
                            aLocation
                                |> Ok

                        actual =
                            aLocation
                                |> GPS.encodeLocation
                                |> Decode.decodeValue GPS.decodeLocation
                    in
                    Expect.equal expected actual
            ]
        , describe "direction"
            [ test "decode North" <|
                \_ ->
                    let
                        expected =
                            North
                                |> Ok

                        actual =
                            """{"direction": "North"}"""
                                |> Decode.decodeString (Decode.field "direction" GPS.decodeDirection)
                    in
                    Expect.equal expected actual
            , fuzz directionFuzzer "encode decode of direction should be inverses" <|
                \aDirection ->
                    let
                        expected =
                            aDirection
                                |> Ok

                        actual =
                            aDirection
                                |> GPS.encodeDirection
                                |> Decode.decodeValue GPS.decodeDirection
                    in
                    Expect.equal expected actual
            ]
        ]


locationFuzzer : Fuzzer Location
locationFuzzer =
    Fuzz.map2 location int int


directionFuzzer : Fuzzer Direction
directionFuzzer =
    oneOf
        [ constant North
        , constant East
        , constant South
        , constant West
        ]
