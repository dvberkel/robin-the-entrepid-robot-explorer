module World.GPSTest exposing (suite)

import Expect
import Fuzz exposing (int)
import Json.Decode as Decode
import Test exposing (Test, describe, fuzz2, test)
import World.GPS as GPS exposing (Direction(..), location)


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
            , fuzz2 int int "encode decode should be inverses" <|
                \x y ->
                    let
                        aLocation =
                            location x y

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
        ]
