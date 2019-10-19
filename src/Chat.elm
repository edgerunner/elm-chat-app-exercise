module Chat exposing (Model, chatView)

import Conversation exposing (Conversation)
import Dict exposing (Dict)
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
        [ convList model
        ]


convList : Model -> Html msg
convList model =
    ul [ class "conversations" ]
        (List.filterMap (convListing (userById model.users)) model.conversations)


convListing : (String -> Maybe User) -> Conversation -> Maybe (Html msg)
convListing user conv =
    Maybe.map
        (\u -> li [] [ userLabel u, unreadBadge conv.unread ])
        (user conv.with)


userLabel : User -> Html msg
userLabel user =
    h4 []
        [ img [ src user.avatar ] []
        , text user.name
        ]


unreadBadge : Int -> Html msg
unreadBadge count =
    case count of
        0 ->
            text ""

        _ ->
            span [ class "unread" ] [ text <| String.fromInt count ]


userById : Dict String User -> String -> Maybe User
userById users id =
    Dict.get id users
