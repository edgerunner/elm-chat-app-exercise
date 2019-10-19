module Conversation exposing (Conversation, get)

import Api
import Json.Decode as D
import RemoteData exposing (WebData)


type alias Conversation =
    { id : String
    , with : String
    , unread : Int
    }


get : (WebData (List Conversation) -> msg) -> Cmd msg
get =
    Api.get "/conversations" decoder


decoder : D.Decoder (List Conversation)
decoder =
    D.map3 Conversation
        (D.field "id" D.string)
        (D.field "with_user_id" D.string)
        (D.field "unread_message_count" D.int)
        |> D.list
