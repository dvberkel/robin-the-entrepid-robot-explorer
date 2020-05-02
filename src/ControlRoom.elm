module ControlRoom exposing (Level, decode, encode, level)

import Browser
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import World exposing (World, world)
import World.GPS exposing (Direction(..), location)
import World.Maze exposing (emptyMaze)
import World.Robot exposing (robot)


main : Program () Model Msg
main =
    Browser.document
        { init = init 0
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Int -> () -> ( Model, Cmd Msg )
init levelIndex _ =
    let
        handler response =
            case response of
                Ok string ->
                    ReceivedLevel levelIndex

                Err error ->
                    LoadError error

        loadCommand =
            Http.get
                { url = "levels/" ++ levelName levelIndex ++ ".json"
                , expect = Http.expectString handler
                }
    in
    ( Loading levelIndex, loadCommand )


type Model
    = Loading Int
    | Loaded Level
    | Failure Http.Error


type alias Level =
    { index : Int
    , world : World
    }


level : Int -> World -> Level
level index aWorld =
    { index = index
    , world = aWorld
    }


encode : Level -> Encode.Value
encode aLevel =
    Encode.object
        [ ( "index", Encode.int aLevel.index )
        , ( "world", World.encode aLevel.world )
        ]


decode : Decoder Level
decode =
    Decode.succeed Level
        |> required "index" Decode.int
        |> required "world" World.decode


view : Model -> Browser.Document Msg
view model =
    let
        body =
            case model of
                Loading levelIndex ->
                    connectingToLevel levelIndex

                Loaded levelIndex ->
                    controlLevel levelIndex

                Failure _ ->
                    connectionFailure
    in
    { title = "Control Room"
    , body = body
    }


connectingToLevel : Int -> List (Html Msg)
connectingToLevel index =
    [ Html.h1 [] [ Html.text "patching into CCTV stream" ] ]


controlLevel : Level -> List (Html Msg)
controlLevel aLevel =
    [ Html.h1 [] [ Html.text <| "Level " ++ levelName aLevel.index ]
    ]


levelName : Int -> String
levelName index =
    let
        prefix =
            if index < 10 then
                "0"

            else
                ""
    in
    prefix ++ String.fromInt index


connectionFailure : List (Html Msg)
connectionFailure =
    [ Html.h1 [] [ Html.text <| "electric interference prevents stable CCTV patch" ] ]


type Msg
    = ReceivedLevel Int
    | LoadError Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ReceivedLevel json ->
            let
                aRobot =
                    robot North (location 0 0)

                aWorld =
                    world emptyMaze aRobot

                aLevel =
                    level 0 aWorld
            in
            ( Loaded aLevel, Cmd.none )

        LoadError error ->
            ( Failure error, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
