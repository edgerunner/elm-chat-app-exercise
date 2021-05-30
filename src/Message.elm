module Message exposing (Message, Messages, Model, body, decoder, from, get, init)

import Api
import IdDict exposing (Id, IdDict)
import Iso8601
import Json.Decode as D
import RemoteData exposing (WebData)
import Time exposing (Posix)


type Message
    = Message Internals


type alias Internals =
    { id : Id
    , from : Id
    , conversation : Id
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
            "/conversations/" ++ convId ++ "/messages"
    in
    Api.get path decoder identity


decoder : D.Decoder Messages
decoder =
    D.map5 Internals
        (D.field "id" D.string)
        (D.field "from_user_id" D.string)
        (D.field "conversation_id" D.string)
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


from : Message -> Id
from =
    internals .from


body : Message -> String
body =
    internals .body
