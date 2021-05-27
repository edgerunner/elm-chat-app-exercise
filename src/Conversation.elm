module Conversation exposing (Conversation, Conversations, checkAndLoad, get)

import Api
import IdDict exposing (Id, IdDict)
import Json.Decode as D
import Message
import Platform.Cmd exposing (Cmd)
import RemoteData exposing (RemoteData(..), WebData)


type alias Conversation =
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
    D.map4 Conversation
        (D.field "id" D.string)
        (D.field "with_user_id" D.string)
        (D.field "unread_message_count" D.int)
        (D.succeed Message.init)
        |> IdDict.decoder


getMessages : (Conversation -> msg) -> Conversation -> ( Conversation, Cmd msg )
getMessages msg conv =
    Message.get conv.id
        |> Cmd.map (\messages -> { conv | messages = messages } |> msg)
        |> Tuple.pair { conv | messages = RemoteData.Loading }


checkAndLoad : (Conversation -> msg) -> Conversation -> ( Conversation, Cmd msg )
checkAndLoad msg conv =
    case conv.messages of
        Success _ ->
            ( conv, Cmd.none )

        Loading ->
            ( conv, Cmd.none )

        NotAsked ->
            getMessages msg conv

        Failure _ ->
            getMessages msg conv
