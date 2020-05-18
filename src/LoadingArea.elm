module LoadingArea exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation exposing (Key)
import Control.ControlRoom as ControlRoom exposing (ControlRoom, controlRoom)
import Control.Level as Level exposing (Level)
import Debug
import Html.Styled as Html exposing (Html)
import Http exposing (Error(..))
import Url exposing (Url)
import Url.Builder as Builder
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
init _ url aKey =
    case Parser.parse levelQuery url of
        Just (Just levelIndex) ->
            ( Loading aKey levelIndex, loadCommand levelIndex )

        Just Nothing ->
            ( Loading aKey 0, loadCommand 0 )

        Nothing ->
            ( Failure aKey <| LevelQuery url, Cmd.none )


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
    = Loading Key Int
    | Loaded Key ControlRoom
    | Failure Key Problem


key : Model -> Key
key model =
    case model of
        Loading aKey _ ->
            aKey

        Loaded aKey _ ->
            aKey

        Failure aKey _ ->
            aKey


type Problem
    = LevelQuery Url
    | Fetch Http.Error
    | Parse String


view : Model -> Browser.Document Msg
view model =
    let
        body =
            case model of
                Loading _ levelIndex ->
                    connectingToLevel levelIndex

                Loaded _ aControlRoom ->
                    ControlRoom.view aControlRoom
                        |> List.map (Html.map ControlRoomMsg)

                Failure _ problem ->
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
            let
                aKey =
                    key model

                url =
                    Builder.relative [] [ Builder.string "level" <| String.fromInt <| Level.levelIndex aLevel ]
            in
            ( Loaded aKey <| controlRoom aLevel, Navigation.replaceUrl aKey url )

        LoadError problem ->
            ( Failure (key model) problem, Cmd.none )

        ControlRoomMsg controlRoomMessage ->
            case model of
                Loaded aKey aControlRoom ->
                    let
                        ( nextControlRoom, cmd ) =
                            ControlRoom.update controlRoomMessage aControlRoom
                    in
                    ( Loaded aKey nextControlRoom, Cmd.map ControlRoomMsg cmd )

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
