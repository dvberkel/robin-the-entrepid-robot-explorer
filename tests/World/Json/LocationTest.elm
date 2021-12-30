module World.Json.LocationTest exposing (..)

import Expect
import Fuzz exposing (Fuzzer, int)
import Json.Decode as Decode
import Test exposing (Test, describe, fuzz, test)
import World.GPS.Location exposing (Location, location)
import World.Json.GPS.Location as Location


suite : Test
suite =
    describe "World"
        [ describe "Json"
            [ describe "GPS"
                [ describe "Location"
                    [ test "decode" <|
                        \_ ->
                            let
                                expected =
                                    location 0 0
                                        |> Ok

                                actual =
                                    """{"x": 0, "y": 0}"""
                                        |> Decode.decodeString Location.decode
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
                                        |> Location.encode
                                        |> Decode.decodeValue Location.decode
                            in
                            Expect.equal expected actual
                    ]
                ]
            ]
        ]


locationFuzzer : Fuzzer Location
locationFuzzer =
    Fuzz.map2 location int int
