module Json.World.GPS.DirectionTest exposing (..)

import Expect
import Fuzz exposing (Fuzzer, constant, oneOf)
import Json.Decode as Decode
import Test exposing (Test, describe, fuzz, test)
import World.GPS.Direction exposing (Direction(..))
import Json.World.GPS.Direction as Direction


suite : Test
suite =
    describe "Json"
        [ describe "World"
            [ describe "GPS"
                [ describe "Direction"
                    [ test "decode North" <|
                        \_ ->
                            let
                                expected =
                                    North
                                        |> Ok

                                actual =
                                    """{"direction": "North"}"""
                                        |> Decode.decodeString (Decode.field "direction" Direction.decode)
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
                                        |> Direction.encode
                                        |> Decode.decodeValue Direction.decode
                            in
                            Expect.equal expected actual
                    ]
                ]
            ]
        ]


directionFuzzer : Fuzzer Direction
directionFuzzer =
    oneOf
        [ constant North
        , constant East
        , constant South
        , constant West
        ]
