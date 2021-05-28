module View exposing (view)

import Chat exposing (Model, Msg, focusedMessages)
import Conversation exposing (Conversation)
import Element exposing (Element, alignTop, centerX, centerY, column, el, fill, height, minimum, padding, paddingXY, pointer, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import IdDict exposing (Id)
import Message exposing (Message)
import Styles exposing (blue, em, eml, gray, red, white)
import User exposing (User)


breakpoint : Int
breakpoint =
    em 25


break : (Model -> Element msg) -> (Model -> Element msg) -> Model -> Element msg
break wide narrow model =
    if Chat.width model > breakpoint then
        wide model

    else
        narrow model


view : Model -> Element Msg
view =
    break fullView focusView


focusView : Model -> Element Msg
focusView model =
    (Chat.focus model
        |> Maybe.map (always conversationView)
        |> Maybe.withDefault conversationList
    )
        model


fullView : Model -> Element Msg
fullView model =
    row
        [ width fill
        , height fill
        , spacing (em 1)
        , padding (em 1)
        , Background.color gray
        ]
        [ fullViewBlock shrink (conversationList model)
        , fullViewBlock fill (conversationView model)
        ]


fullViewBlock : Element.Length -> Element Msg -> Element Msg
fullViewBlock w =
    el
        [ alignTop
        , Background.color Styles.white
        , height fill
        , width w
        ]


selectionAttributes : Conversation -> Maybe Id -> List (Element.Attribute msg)
selectionAttributes conv focus =
    if
        focus
            |> Maybe.map ((==) conv.id)
            |> Maybe.withDefault False
    then
        [ Background.color blue ]

    else
        []


conversationList : Model -> Element Msg
conversationList model =
    column
        [ width fill
        , height shrink
        , alignTop
        ]
        (List.filterMap
            (\conv ->
                let
                    user =
                        Chat.user conv.with model

                    listing justUser =
                        el
                            ([ Events.onClick (Chat.msg.focusConversation conv)
                             , width fill
                             , pointer
                             ]
                                ++ selectionAttributes conv (Chat.focus model)
                            )
                            (convListing conv justUser)
                in
                Maybe.map listing user
            )
            (Chat.conversations model)
        )


conversationView : Model -> Element Msg
conversationView model =
    el
        [ width fill
        , height fill
        , Events.onClick Chat.msg.blurConversation
        ]
        (focusedMessages model
            |> Maybe.map messagesView
            |> Maybe.withDefault (blob "Select conversation")
        )


messagesView : List ( Message, User ) -> Element msg
messagesView =
    List.map messageView >> column [ padding (em 1), spacing (em 0.5) ]


messageView : ( Message, User ) -> Element msg
messageView ( message, user ) =
    row [ spacing (em 0.5) ] [ User.avatar 1 user, text message.body ]


blob : String -> Element msg
blob string =
    el [ centerX, centerY ] (text string)


convListing : Conversation -> User -> Element msg
convListing conv user =
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
