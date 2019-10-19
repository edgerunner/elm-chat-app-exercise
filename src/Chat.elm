module Chat exposing (Model, chatView)

import Conversation exposing (Conversation)
import Dict exposing (Dict)
import Element exposing (Element, alignRight, centerY, el, fill, padding, rgb255, row, spacing, text, width)
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
        [ convList model
        ]


convList : Model -> Html msg
convList model =
    ul [ class "conversations" ]
        (List.filterMap (convListing (userById model.users)) model.conversations)


convListing : (String -> Maybe User) -> Conversation -> Maybe (Html msg)
convListing user conv =
    Maybe.map
        (\u -> li [] [ userLabel u, Element.layout [] (unreadBadge conv.unread) ])
        (user conv.with)


userLabel : User -> Html msg
userLabel user =
    h4 []
        [ img [ src user.avatar ] []
        , Html.text user.name
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
