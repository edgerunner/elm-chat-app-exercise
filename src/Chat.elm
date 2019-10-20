module Chat exposing (Model, Msg, init, update, view)

import Conversation exposing (Conversation)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import User exposing (User)


type Model
    = Model ModelRecord


type alias ModelRecord =
    { users : Dict String User
    , conversations : List Conversation
    , focus : Focus
    }


map : (ModelRecord -> a) -> Model -> a
map fn (Model model) =
    fn model


type Msg
    = FocusConversation Conversation
    | BlurConversation


type Focus
    = FullView
    | ListView
    | ConversationView Conversation


init : List User -> List Conversation -> Model
init users conversations =
    Model
        { users = List.foldl (\user -> Dict.insert user.id user) Dict.empty users
        , conversations = conversations
        , focus = ListView
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FocusConversation conv ->
            ( focusConversation conv model, Cmd.none )

        _ ->
            ( model, Cmd.none )


focusConversation : Conversation -> Model -> Model
focusConversation conv model =
    let
        rest =
            map identity model
    in
    Model
        { rest | focus = ConversationView conv }


view : Model -> Element Msg
view model =
    case map .focus model of
        ListView ->
            listView model

        ConversationView _ ->
            conversationView model

        _ ->
            Debug.todo "implement the other views"


listView : Model -> Element Msg
listView model =
    row
        [ width fill
        , height fill
        ]
        [ convList model
        ]


convList : Model -> Element Msg
convList (Model model) =
    column
        [ width fill
        , height shrink
        , alignTop
        ]
        (List.filterMap (convListing (userById model.users)) model.conversations)


convListing : (String -> Maybe User) -> Conversation -> Maybe (Element Msg)
convListing user conv =
    Maybe.map
        (\u ->
            row
                [ width fill
                , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                , spacing (em 1)
                , paddingXY (em 0.25) 0
                , Events.onClick <| FocusConversation conv
                ]
                [ userLabel u
                , unreadBadge conv.unread
                , text "❯"
                ]
        )
        (user conv.with)


userLabel : User -> Element msg
userLabel user =
    row
        [ width fill
        , height (eml 3)
        , centerY
        , spacing (em 0.5)
        ]
        [ Element.image
            [ height (eml 2.4)
            , width (eml 2.4)
            , Border.rounded (em 1.2)
            , clip
            ]
            { src = user.avatar, description = user.name }
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


conversationView : Model -> Element msg
conversationView (Model model) =
    el
        [ width fill
        , height fill
        , Background.color gray
        ]
        (text "Conversation view placeholder")


white : Color
white =
    rgb255 255 255 255


gray : Color
gray =
    rgb255 192 192 192


red : Color
red =
    rgb255 255 0 0


em : Float -> Int
em =
    truncate << (*) 18


eml : Float -> Length
eml =
    px << em
