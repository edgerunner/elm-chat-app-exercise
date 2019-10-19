module Chat exposing (Model, chatView)

import Conversation exposing (Conversation)
import Dict exposing (Dict)
import Element exposing (Element, alignRight, centerY, column, el, fill, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (..)
import Html.Attributes exposing (class, src)
import User exposing (User)


type alias Model =
    { users : Dict String User
    , conversations : List Conversation
    , currentUser : String
    }


chatView : Model -> Html msg
chatView model =
    div [ class "chat" ]
        [ Element.layout [] (convList model)
        ]


convList : Model -> Element msg
convList model =
    column []
        (List.filterMap (convListing (userById model.users)) model.conversations)


convListing : (String -> Maybe User) -> Conversation -> Maybe (Element msg)
convListing user conv =
    Maybe.map
        (\u ->
            row []
                [ userLabel u
                , unreadBadge conv.unread
                ]
        )
        (user conv.with)


userLabel : User -> Element msg
userLabel user =
    row []
        [ Element.image [] { src = user.avatar, description = user.name }
        , Element.text user.name
        ]


unreadBadge : Int -> Element msg
unreadBadge count =
    case count of
        0 ->
            Element.none

        _ ->
            String.fromInt count
                |> Element.text
                |> el []


userById : Dict String User -> String -> Maybe User
userById users id =
    Dict.get id users
