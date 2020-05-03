module ControlRoom exposing (Level, decode, encode, level)

import Browser
import Debug
import Html exposing (Html)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import World exposing (World)


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
                Ok aLevel ->
                    ReceivedLevel aLevel

                Err error ->
                    case error of
                        BadBody reason ->
                            LoadError <| Parse reason

                        _ ->
                            LoadError <| Fetch error

        loadCommand =
            Http.get
                { url = "levels/" ++ levelName levelIndex ++ ".json"
                , expect = Http.expectJson handler decode
                }
    in
    ( Loading levelIndex, loadCommand )


type Model
    = Loading Int
    | Loaded Level
    | Failure Problem


type Problem
    = Fetch Http.Error
    | Parse String


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

                Failure problem ->
                    connectionFailure problem
    in
    { title = "Control Room"
    , body = body
    }


connectingToLevel : Int -> List (Html Msg)
connectingToLevel _ =
    [ Html.h1 [] [ Html.text "patching into CCTV stream" ] ]


controlLevel : Level -> List (Html Msg)
controlLevel aLevel =
    [ Html.h1 [] [ Html.text <| "Level " ++ levelName aLevel.index ]
    , World.view aLevel.world
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


connectionFailure : Problem -> List (Html Msg)
connectionFailure problem =
    [ Html.h1 [] [ Html.text <| "electric interference prevents stable CCTV patch"]
    , Html.p [] [ Html.text <| Debug.toString problem ]
    ]


type Msg
    = ReceivedLevel Level
    | LoadError Problem


update : Msg -> Model -> ( Model, Cmd Msg )
update message _ =
    case message of
        ReceivedLevel aLevel ->
            ( Loaded aLevel, Cmd.none )

        LoadError problem ->
            ( Failure problem, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
