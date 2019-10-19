module Chat exposing (Model, chatView)

import Conversation exposing (Conversation)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import User exposing (User)


type alias Model =
    { users : Dict String User
    , conversations : List Conversation
    , currentUser : String
    }


chatView : Model -> Element msg
chatView model =
    row
        []
        [ convList model
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


userById : Dict String User -> String -> Maybe User
userById users id =
    Dict.get id users


white : Color
white =
    rgb255 255 255 255


red : Color
red =
    rgb255 255 0 0


em : Float -> Int
em =
    truncate << (*) 18


eml : Float -> Length
eml =
    px << em
