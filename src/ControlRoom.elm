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
        loadCommand =
            Http.get
                { url = "levels/" ++ levelName level ++ ".json"
                , expect = Http.expectString (\_ -> ReceivedLevel level)
                }
    in
    ( Loading 0, loadCommand )


type Model
    = Loading Int
    | Loaded Level


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


type Msg
    = ReceivedLevel Int


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ReceivedLevel level ->
            ( Loaded { level = level }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
