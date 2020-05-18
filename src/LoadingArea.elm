module LoadingArea exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation exposing (Key)
import Control.ControlRoom as ControlRoom exposing (ControlRoom, controlRoom)
import Control.Level as Level exposing (Level)
import Debug
import Html.Styled as Html exposing (Html)
import Http exposing (Error(..))
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init 0
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = onUrlChange
        , onUrlRequest = onUrlRequest
        }


init : Int -> () -> Url -> Key -> ( Model, Cmd Msg )
init levelIndex _ _ key =
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
                { url = "/levels/" ++ Level.name levelIndex ++ ".json"
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
                        |> List.map (Html.map ControlRoomMsg)

                Failure problem ->
                    connectionFailure problem
    in
    { title = "Control Room"
    , body = body |> List.map Html.toUnstyled
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
    = Idle 
    | ReceivedLevel Level
    | LoadError Problem
    | ControlRoomMsg ControlRoom.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Idle ->
            ( model, Cmd.none )

        ReceivedLevel aLevel ->
            ( Loaded <| controlRoom aLevel, Cmd.none )

        LoadError problem ->
            ( Failure problem, Cmd.none )

        ControlRoomMsg controlRoomMessage ->
            case model of
                Loaded aControlRoom ->
                    let
                        ( nextControlRoom, cmd ) =
                            ControlRoom.update controlRoomMessage aControlRoom
                    in
                    ( Loaded nextControlRoom, Cmd.map ControlRoomMsg cmd )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


onUrlChange : Url -> Msg
onUrlChange _ =
    Idle

onUrlRequest : UrlRequest -> Msg
onUrlRequest _ =
    Idle