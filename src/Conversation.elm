module Conversation exposing (Conversation, Conversations, checkAndLoad, get, id, messages, unread, update, with)

import Api
import Dict
import IdDict exposing (Id, IdDict)
import Json.Decode as D
import Message
import Platform.Cmd exposing (Cmd)
import RemoteData exposing (RemoteData(..), WebData)


type Conversation
    = Conversation Internals


type alias Internals =
    { id : Id
    , with : Id
    , unread : Int
    , messages : Message.Model
    }


type alias Conversations =
    IdDict Conversation


get : (WebData Conversations -> msg) -> Cmd msg
get =
    Api.get "/conversations" decoder


decoder : D.Decoder Conversations
decoder =
    D.map4 Internals
        (D.field "id" D.string)
        (D.field "with_user_id" D.string)
        (D.field "unread_message_count" D.int)
        (D.succeed Message.init)
        |> D.map Conversation
        |> IdDict.decoder id


internals : (Internals -> a) -> Conversation -> a
internals extract (Conversation internals_) =
    extract internals_


id : Conversation -> Id
id =
    internals .id


with : Conversation -> Id
with =
    internals .with


unread : Conversation -> Int
unread =
    internals .unread


messages : Conversation -> Message.Model
messages =
    internals .messages


update : Conversation -> IdDict Conversation -> IdDict Conversation
update conv =
    Dict.update (id conv) (always <| Just conv)


getMessages : (Conversation -> msg) -> Conversation -> ( Conversation, Cmd msg )
getMessages msg (Conversation conv) =
    Message.get conv.id
        |> Cmd.map (\messages_ -> Conversation { conv | messages = messages_ } |> msg)
        |> Tuple.pair (Conversation { conv | messages = RemoteData.Loading })


checkAndLoad : (Conversation -> msg) -> Conversation -> ( Conversation, Cmd msg )
checkAndLoad msg conversation =
    case internals .messages conversation of
        Success _ ->
            ( conversation, Cmd.none )

        Loading ->
            ( conversation, Cmd.none )

        NotAsked ->
            getMessages msg conversation

        Failure _ ->
            getMessages msg conversation
