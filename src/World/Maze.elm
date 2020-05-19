module World.Maze exposing (Maze, Tile(..), boundingBox, decode, emptyMaze, encode, insertRectangle, insertTile, tileAt, view)

import Dict exposing (Dict)
import Html.Styled exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attribute
import World.GPS as GPS exposing (Location, location)


type Maze
    = Maze (Dict Int (Dict Int Tile))


type Tile
    = Floor
    | Pit
    | Wall
    | Target


emptyMaze : Maze
emptyMaze =
    Maze Dict.empty


insertRectangle : ( Location, Location ) -> Tile -> Maze -> Maze
insertRectangle aBoundingBox element aMaze =
    locationsIn aBoundingBox
        |> List.foldl (swap insertTile <| element) aMaze


locationsIn : ( Location, Location ) -> List Location
locationsIn ( lowerLeft, upperRight ) =
    let
        ( llx, lly ) =
            GPS.coordinates2D lowerLeft

        ( urx, ury ) =
            GPS.coordinates2D upperRight

        xs =
            List.range llx urx

        ys =
            List.range lly ury
    in
    cartesianProduct xs ys
        |> List.map (unpack GPS.location)


unpack : (a -> b -> c) -> ( a, b ) -> c
unpack f ( a, b ) =
    f a b


swap : (a -> b -> c) -> b -> a -> c
swap f b a =
    f a b


cartesianProduct : List a -> List b -> List ( a, b )
cartesianProduct xs ys =
    let
        columnOf x =
            ys
                |> List.map (\y -> ( x, y ))
    in
    xs
        |> List.concatMap columnOf


insertTile : Location -> Tile -> Maze -> Maze
insertTile location tile (Maze locations) =
    let
        ( x, y ) =
            GPS.coordinates2D location

        insert : Maybe (Dict Int Tile) -> Maybe (Dict Int Tile)
        insert dictionary =
            dictionary
                |> Maybe.withDefault Dict.empty
                |> Dict.insert y tile
                |> Just
    in
    locations
        |> Dict.update x insert
        |> Maze


tileAt : Location -> Maze -> Tile
tileAt location (Maze dictionary) =
    let
        ( x, y ) =
            GPS.coordinates2D location
    in
    dictionary
        |> Dict.get x
        |> Maybe.withDefault Dict.empty
        |> Dict.get y
        |> Maybe.withDefault Pit


encode : Maze -> Encode.Value
encode (Maze dictionary) =
    let
        toYTiles : Int -> Dict Int Tile -> List ( Int, Tile )
        toYTiles _ =
            Dict.toList

        toXYTiles : ( Int, List ( Int, Tile ) ) -> List ( ( Int, Int ), Tile )
        toXYTiles ( x, ys ) =
            List.map (\( y, tile ) -> ( ( x, y ), tile )) ys

        toObjectPairs ( ( x, y ), tile ) =
            ( String.join "," <| List.map String.fromInt [ x, y ], encodeTile tile )
    in
    dictionary
        |> Dict.map toYTiles
        |> Dict.toList
        |> List.concatMap toXYTiles
        |> List.map toObjectPairs
        |> Encode.object


encodeTile : Tile -> Encode.Value
encodeTile tile =
    let
        value =
            case tile of
                Floor ->
                    "Floor"

                Pit ->
                    "Pit"

                Wall ->
                    "Wall"

                Target ->
                    "Target"
    in
    Encode.string value


decode : Decoder Maze
decode =
    let
        parse ( input, tile ) =
            input
                |> parseLocation
                |> Result.map (\aLocation -> ( aLocation, tile ))

        insert ( aLocation, aTile ) aMaze =
            insertTile aLocation aTile aMaze

        toMaze xs =
            xs
                |> List.foldl insert emptyMaze
    in
    Decode.keyValuePairs decodeTile
        |> parseMap (\( input, _ ) -> input) parse
        |> Decode.map toMaze


parseMap : (a -> String) -> (a -> Result e b) -> Decoder (List a) -> Decoder (List b)
parseMap toString parse xss =
    let
        decoder xs =
            case xs of
                [] ->
                    Decode.succeed []

                ( index, h ) :: tail ->
                    case parse h of
                        Ok y ->
                            decoder tail
                                |> Decode.map (\ys -> ( index, y ) :: ys)

                        Err _ ->
                            Decode.fail <| "could not parse element '" ++ toString h ++ "' at index: " ++ String.fromInt index
    in
    xss
        |> Decode.map enumerate
        |> Decode.andThen decoder
        |> Decode.map denumerate


enumerate : List a -> List ( Int, a )
enumerate =
    indexedEnumerate 0 []


denumerate : List ( Int, a ) -> List a
denumerate =
    tailRecursiveDenumerate []


tailRecursiveDenumerate : List a -> List ( Int, a ) -> List a
tailRecursiveDenumerate accumulator xs =
    case xs of
        [] ->
            List.reverse accumulator

        ( _, h ) :: tail ->
            tailRecursiveDenumerate (h :: accumulator) tail


indexedEnumerate : Int -> List ( Int, a ) -> List a -> List ( Int, a )
indexedEnumerate index accumulator xs =
    case xs of
        [] ->
            List.reverse accumulator

        h :: tail ->
            indexedEnumerate (index + 1) (( index, h ) :: accumulator) tail


decodeTile : Decoder Tile
decodeTile =
    let
        toTile input =
            case input of
                "Floor" ->
                    Decode.succeed Floor

                "Pit" ->
                    Decode.succeed Pit

                "Wall" ->
                    Decode.succeed Wall

                "Target" ->
                    Decode.succeed Target

                _ ->
                    Decode.fail <| "'" ++ input ++ "' is not a tile"
    in
    Decode.string
        |> Decode.andThen toTile


parseLocation : String -> Result LocationParseError Location
parseLocation input =
    let
        coordinates =
            input
                |> String.split ","
                |> List.map String.toInt
    in
    case coordinates of
        [ Just x, Just y ] ->
            Ok <| location x y

        _ ->
            Err <| NotALocation input


type LocationParseError
    = NotALocation String


view : Maze -> Html msg
view aMaze =
    let
        toTile aLocation =
            ( aLocation, tileAt aLocation aMaze )

        tiles =
            aMaze
                |> boundingBox
                |> Maybe.map locationsIn
                |> Maybe.withDefault []
                |> List.map toTile
                |> List.map viewTile
    in
    Svg.g [] tiles


boundingBox : Maze -> Maybe ( Location, Location )
boundingBox (Maze dictionary) =
    let
        switch ( y, x ) =
            ( x, y )

        pairUp ( x, ys ) =
            ys
                |> Dict.map (\_ _ -> x)
                |> Dict.toList
                |> List.map switch

        pairs =
            dictionary
                |> Dict.toList
                |> List.concatMap pairUp

        extremize extremizer ( x, y ) ( ex, ey ) =
            ( extremizer x ex, extremizer y ey )

        gather p =
            ( List.foldl (extremize min) p pairs
            , List.foldl (extremize max) p pairs
            )

        toBoundingBox ( ( llx, lly ), ( urx, ury ) ) =
            ( location llx lly, location urx ury )
    in
    pairs
        |> List.head
        |> Maybe.map gather
        |> Maybe.map toBoundingBox


viewTile : ( Location, Tile ) -> Html msg
viewTile ( aLocation, tile ) =
    let
        fillColor =
            case tile of
                Pit ->
                    "black"

                Floor ->
                    "blue"

                Wall ->
                    "red"

                Target ->
                    "green"

        ( x, y ) =
            GPS.coordinates2D aLocation
    in
    Svg.rect
        [ Attribute.fill fillColor
        , Attribute.width "1"
        , Attribute.height "1"
        , Attribute.rx "0.1"
        , Attribute.ry "0.1"
        , Attribute.x <| String.fromInt x
        , Attribute.y <| String.fromInt y
        ]
        []
