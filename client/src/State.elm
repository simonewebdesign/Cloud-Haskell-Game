module State exposing (init, update, subscriptions)

import Json.Decode as D exposing (Decoder)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as E
import RemoteData exposing (RemoteData(..))
import Response exposing (..)
import Types exposing (..)
import WebSocket
import Keyboard


websocketEndpoint : String
websocketEndpoint =
    "ws://game.clearercode.com:8000"


init : Response Model Msg
init =
    ( NotAsked, Cmd.none )


update : Msg -> Model -> Response Model Msg
update msg model =
    case msg of
        KeepAlive ->
            ( model, Cmd.none )

        Receive response ->
            ( response
            , Cmd.none
            )

        GameMsg submsg ->
            ( model
            , encodeGameMsg submsg
                |> E.encode 0
                |> WebSocket.send websocketEndpoint
            )

        KeyMsg code ->
            let _ = Debug.log "keycode" code in
            case code of
                56 ->
                    -- up
                    update (GameMsg (Move (Coords 0 -1))) model
                54 ->
                    -- right
                    update (GameMsg (Move (Coords 1 0))) model
                53 ->
                    -- down
                    update (GameMsg (Move (Coords 0 1))) model
                52 ->
                    -- left
                    update (GameMsg (Move (Coords -1 0))) model
                _ ->
                    ( model, Cmd.none )


encodeGameMsg : GameMsg -> E.Value
encodeGameMsg msg =
    E.object
        <| case msg of
            SetName string ->
                [ ( "tag", E.string "SetName" )
                , ( "contents", E.string string )
                ]

            SetColor string ->
                [ ( "tag", E.string "SetColor" )
                , ( "contents", E.string string )
                ]

            Move to ->
                [ ( "tag", E.string "Move" )
                , ( "contents"
                  , E.object
                        [ ( "x", E.float to.x )
                        , ( "y", E.float to.y )
                        ]
                  )
                ]


decodeCoords : Decoder Coords
decodeCoords =
    decode Coords
        |> required "x" D.float
        |> required "y" D.float


decodeGps : Decoder Gps
decodeGps =
    decode Gps
        |> required "distance" D.float
        |> required "position" decodeCoords


decodePlayer : Decoder Player
decodePlayer =
    decode Player
        |> required "name" D.string
        |> required "score" D.int
        |> required "position" decodeCoords
        |> required "color" (D.maybe D.string)


decodeBoard : Decoder Board
decodeBoard =
    decode Board
        |> required "gpss" (D.list decodeGps)
        |> required "players" (D.list decodePlayer)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen websocketEndpoint (D.decodeString decodeBoard >> RemoteData.fromResult >> Receive)
        , WebSocket.keepAlive websocketEndpoint
            |> Sub.map (always KeepAlive)
        , Keyboard.presses KeyMsg
        ]
