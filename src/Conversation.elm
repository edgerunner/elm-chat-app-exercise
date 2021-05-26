module Conversation exposing (Conversation, Conversations, get, getMessages, listing, view)

import Api
import Element exposing (Element, el, fill, height, minimum, padding, paddingXY, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import IdDict exposing (Id, IdDict)
import Json.Decode as D
import Message
import Platform.Cmd exposing (Cmd)
import RemoteData exposing (WebData)
import Styles exposing (em, eml, gray, red, white)
import User exposing (User)


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


listing : Conversation -> User -> Element msg
listing conv user =
    row
        [ width fill
        , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
        , Border.color gray
        , spacing (em 1)
        , paddingXY (em 0.25) 0
        ]
        [ User.label user
        , unreadBadge conv.unread
        , text "❯"
        ]


unreadBadge : Int -> Element msg
unreadBadge count =
    case count of
        0 ->
            Element.none

        _ ->
            String.fromInt count
                |> Element.text
                |> el
                    [ Font.bold
                    , Font.size (em 1)
                    , Font.color white
                    , Border.rounded (em 1)
                    , Background.color red
                    , width (minimum (em 1.5) shrink)
                    , height (eml 1.5)
                    , padding (em 0.25)
                    , Font.center
                    ]


getMessages : (Conversation -> msg) -> Conversation -> Cmd msg
getMessages msg conv =
    Message.get conv.id
        |> Cmd.map (\messages -> { conv | messages = messages } |> msg)


view : Conversation -> Element msg
view conv =
    Message.view conv.messages
