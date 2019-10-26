module Conversation exposing (Conversation, convListing, get)

import Api
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Json.Decode as D
import Messages
import RemoteData exposing (WebData)
import Styles exposing (em, eml, red, white)
import User exposing (User, userLabel)


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
    let
        specificIdDecoder id =
            D.map4 Conversation
                (D.succeed id)
                (D.field "with_user_id" D.string)
                (D.field "unread_message_count" D.int)
                (D.succeed <| Messages.init id )
                |> D.list

    in
        D.field "id" D.string
            |> D.andThen specificIdDecoder
    


convListing : Conversation -> User -> Element msg
convListing conv user =
    row
        [ width fill
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , spacing (em 1)
        , paddingXY (em 0.25) 0
        ]
        [ userLabel user
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
