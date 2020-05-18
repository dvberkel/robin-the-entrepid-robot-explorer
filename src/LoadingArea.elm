module LoadingArea exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation exposing (Key)
import Control.ControlRoom as ControlRoom exposing (ControlRoom, controlRoom)
import Control.Level as Level exposing (Level)
import Debug
import Html.Styled as Html exposing (Html)
import Http exposing (Error(..))
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, s)
import Url.Parser.Query as Query


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = onUrlChange
        , onUrlRequest = onUrlRequest
        }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    case Parser.parse levelQuery url of
        Just (Just levelIndex) ->
            ( Loading levelIndex, loadCommand levelIndex )

        Just Nothing ->
            ( Loading 0, loadCommand 0 )

        Nothing ->
            ( Failure <| LevelQuery url, Cmd.none )


levelQuery : Parser (Maybe Int -> a) a
levelQuery =
    s "src" </> s "LoadingArea.elm" <?> Query.int "level"


loadCommand : Int -> Cmd Msg
loadCommand levelIndex =
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
    in
    Http.get
        { url = "/levels/" ++ Level.name levelIndex ++ ".json"
        , expect = Http.expectJson handler Level.decode
        }


type Model
    = Loading Int
    | Loaded ControlRoom
    | Failure Problem


type Problem
    = LevelQuery Url
    | Fetch Http.Error
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
