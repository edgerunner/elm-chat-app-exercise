module Conversation exposing (Conversation, Msg, get, getMessages, listing, update, view)

import Api
import Element exposing (Element, el, fill, height, minimum, padding, paddingXY, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Json.Decode as D
import Messages
import RemoteData exposing (WebData)
import Styles exposing (em, eml, gray, red, white)
import User exposing (User)


type alias Conversation =
    { id : String
    , with : String
    , unread : Int
    , messages : Messages.Model
    }


get : (WebData (List Conversation) -> msg) -> Cmd msg
get =
    Api.get "/conversations" decoder


decoder : D.Decoder (List Conversation)
decoder =
    D.map4 Conversation
        (D.field "id" D.string)
        (D.field "with_user_id" D.string)
        (D.field "unread_message_count" D.int)
        (D.succeed Messages.init)
        |> D.list


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


type Msg
    = MessagesMsg Conversation Messages.Msg


getMessages : Conversation -> Cmd Msg
getMessages conv =
    Messages.get conv.id
        |> Cmd.map (MessagesMsg conv)


update : Msg -> List Conversation -> ( List Conversation, Cmd Msg )
update msg list =
    case msg of
        MessagesMsg conv messageMsg ->
            updateMessagesFor conv messageMsg list


updateMessagesFor : Conversation -> Messages.Msg -> List Conversation -> ( List Conversation, Cmd Msg )
updateMessagesFor conv msg list =
    List.map
        (\c ->
            if c == conv then
                let
                    ( messages, cmd ) =
                        Messages.update msg conv.messages
                in
                ( { conv | messages = messages }, cmd )

            else
                ( c, Cmd.none )
        )
        list
        |> List.unzip
        |> Tuple.mapSecond Cmd.batch
        |> Tuple.mapSecond (Cmd.map (MessagesMsg conv))


view : Conversation -> Element msg
view conv =
    Messages.view conv.messages
