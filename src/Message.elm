module Message exposing (Message, Messages, Model, asList, body, decoder, get, incoming, init)

import Api
import Id exposing (Id)
import IdDict exposing (IdDict)
import Iso8601
import Json.Decode as D
import RemoteData exposing (WebData)
import Time exposing (Posix)


type Message
    = Message Internals


type alias Internals =
    { id : Id
    , incoming : Bool
    , body : String
    , time : Posix
    }


type alias Model =
    WebData Messages


init : Model
init =
    RemoteData.NotAsked


type alias Messages =
    IdDict Message


get : Id -> Cmd Model
get convId =
    let
        path =
            "/conversations/" ++ Id.toString convId ++ "/messages"
    in
    Api.get path decoder identity


decoder : D.Decoder Messages
decoder =
    D.map4 Internals
        (D.field "id" Id.decoder)
        (D.field "incoming" D.bool)
        (D.field "body" D.string)
        (D.field "created_at" Iso8601.decoder)
        |> D.map Message
        |> IdDict.decoder id


internals : (Internals -> a) -> Message -> a
internals extract (Message internals_) =
    extract internals_


id : Message -> Id
id =
    internals .id


incoming : Message -> Bool
incoming =
    internals .incoming


body : Message -> String
body =
    internals .body


asList : Messages -> List Message
asList =
    IdDict.toList
