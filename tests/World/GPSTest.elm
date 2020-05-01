module World.GPSTest exposing (suite)

import Expect
import Fuzz exposing (int)
import Json.Decode as Decode
import Test exposing (Test, describe, fuzz, fuzz2, test)
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
            , fuzz2 int int "encode decode of location should be inverses" <|
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
            , fuzz (Fuzz.oneOf [ Fuzz.constant North, Fuzz.constant East, Fuzz.constant South, Fuzz.constant West ]) "encode decode of direction should be inverses" <|
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
