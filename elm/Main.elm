port module Main exposing (..)

import Dict as Dict exposing (Dict)
import Html exposing (Html, div, button, text, program)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder, field, string)
import Random



-- MODEL
type alias ContinuationKey = Int

type alias Model =
    { acquireTokenContinuations: Dict.Dict ContinuationKey (JwtToken -> Cmd Msg)
    , continuationKey: ContinuationKey
    , latestToken: JwtToken
    , answer: Int
    }

init : ( Model, Cmd Msg )
init
    = ( { acquireTokenContinuations = Dict.empty
        , continuationKey = 0
        , latestToken = ""
        , answer = 0
      }
    , Cmd.none )

type alias JwtToken = String

type alias Token =
    { token: JwtToken
    , continuationKey: ContinuationKey
    }

tokenDecoder : Decoder Token
tokenDecoder =
    Decode.map2 Token
        ( Decode.field "token" Decode.string )
        ( Decode.field "continuationKey" Decode.int )



-- MESSAGES
type Msg
    = TokenAcquired (Result String Token)
    | ButtonClicked
    | RandomResult Int



-- VIEW
view : Model -> Html Msg
view model =
    div []
    [ div []
        [ text "What's the answer to life, the universe and everything? "
        , text (toString model.answer) ]
    , button [ onClick ButtonClicked ] [ text "Get answer" ]
    ]



-- UPDATES
port acquireToken : Int -> Cmd msg

randomRoll : Int -> Int -> JwtToken -> Cmd Msg
randomRoll min max token =
    Random.generate RandomResult ( Random.int min max )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ButtonClicked ->
            let
                continuationKey = model.continuationKey + 1
                continuation = randomRoll 42 42
                acquireTokenContinuations = Dict.insert continuationKey continuation model.acquireTokenContinuations
            in
                ( { model
                    | continuationKey = continuationKey
                    , acquireTokenContinuations = acquireTokenContinuations }
                , acquireToken continuationKey )
        TokenAcquired (Err err) ->
            ( model, Cmd.none )
        TokenAcquired (Ok token) ->
            let
                continuationKey = token.continuationKey
                continuation = Dict.get continuationKey model.acquireTokenContinuations
                continuationResult = case continuation of
                    Nothing -> Cmd.none
                    Just c -> c token.token
            in
                ( { model | latestToken = token.token }, continuationResult )
        RandomResult answer ->
            ( { model | answer = answer }, Cmd.none )



-- SUBSCRIPTIONS
port tokens : (Decode.Value -> msg) -> Sub msg

subscriptions :  Model -> Sub Msg
subscriptions model =
    tokens tokenAcquired

tokenAcquired : Decode.Value -> Msg
tokenAcquired json =
    TokenAcquired (Decode.decodeValue tokenDecoder json)



-- MAIN
main : Program Never Model Msg
main =
    program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }