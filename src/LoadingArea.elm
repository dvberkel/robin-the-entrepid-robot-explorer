module LoadingArea exposing (main)

import Browser
import Control.ControlRoom as ControlRoom exposing (ControlRoom, controlRoom)
import Control.Level as Level exposing (Level)
import Debug
import Html exposing (Html)
import Http exposing (Error(..))


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
                { url = "levels/" ++ Level.name levelIndex ++ ".json"
                , expect = Http.expectJson handler Level.decode
                }
    in
    ( Loading levelIndex, loadCommand )


type Model
    = Loading Int
    | Loaded ControlRoom
    | Failure Problem


type Problem
    = Fetch Http.Error
    | Parse String


view : Model -> Browser.Document Msg
view model =
    let
        body =
            case model of
                Loading levelIndex ->
                    connectingToLevel levelIndex

                Loaded aControlRoom ->
                    ControlRoom.view aControlRoom

                Failure problem ->
                    connectionFailure problem
    in
    { title = "Control Room"
    , body = body
    }


connectingToLevel : Int -> List (Html Msg)
connectingToLevel _ =
    [ Html.h1 [] [ Html.text "patching into CCTV stream" ] ]


connectionFailure : Problem -> List (Html Msg)
connectionFailure problem =
    [ Html.h1 [] [ Html.text <| "electric interference prevents stable CCTV patch" ]
    , Html.p [] [ Html.text <| Debug.toString problem ]
    ]


type Msg
    = ReceivedLevel Level
    | LoadError Problem


update : Msg -> Model -> ( Model, Cmd Msg )
update message _ =
    case message of
        ReceivedLevel aLevel ->
            ( Loaded <| controlRoom aLevel, Cmd.none )

        LoadError problem ->
            ( Failure problem, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
