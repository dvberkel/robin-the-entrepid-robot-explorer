module ControlRoom exposing (..)

import Browser
import Html exposing (Html)
import Http


main : Program () Model Msg
main =
    Browser.document
        { init = init 0
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Int -> () -> ( Model, Cmd Msg )
init level _ =
    let
        handler response =
            case response of
                Ok string ->
                    ReceivedLevel level

                Err error ->
                    LoadError error

        loadCommand =
            Http.get
                { url = "levels/" ++ levelName level ++ ".json"
                , expect = Http.expectString handler
                }
    in
    ( Loading level, loadCommand )


type Model
    = Loading Int
    | Loaded Level
    | Failure Http.Error


type alias Level =
    { level : Int
    }


view : Model -> Browser.Document Msg
view model =
    let
        body =
            case model of
                Loading level ->
                    connectingToLevel level

                Loaded level ->
                    controlLevel level

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
controlLevel { level } =
    [ Html.h1 [] [ Html.text <| "Level " ++ levelName level ]
    ]


levelName : Int -> String
levelName level =
    let
        prefix =
            if level < 10 then
                "0"

            else
                ""
    in
    prefix ++ String.fromInt level


connectionFailure : List (Html Msg)
connectionFailure =
    [ Html.h1 [] [ Html.text <| "electric interference prevents stable CCTV patch" ] ]


type Msg
    = ReceivedLevel Int
    | LoadError Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ReceivedLevel level ->
            ( Loaded { level = level }, Cmd.none )

        LoadError error ->
            ( Failure error, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
