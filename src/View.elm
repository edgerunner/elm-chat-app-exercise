module View exposing (avatar, userLabel, view)

import Chat exposing (Model, Msg, focusedMessages)
import Conversation exposing (Conversation)
import Element exposing (Element, alignTop, centerX, centerY, clip, column, el, fill, height, minimum, padding, paddingXY, pointer, row, shrink, spacing, text, width)
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
        (List.map
            (\conv ->
                el
                    ([ Events.onClick (Chat.msg.focusConversation conv)
                     , width fill
                     , pointer
                     ]
                        ++ selectionAttributes conv (Chat.focus model)
                    )
                    (Chat.user conv.with model
                        |> Maybe.withDefault User.unknown
                        |> convListing conv
                    )
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
            |> Maybe.map (List.map (messageWithUser model))
            |> Maybe.map messagesView
            |> Maybe.withDefault (blob "Select conversation")
        )


messageWithUser : Model -> Message -> ( Message, User )
messageWithUser model message =
    ( message, Chat.messageFrom message model )


messagesView : List ( Message, User ) -> Element msg
messagesView =
    List.map messageView >> column [ padding (em 1), spacing (em 0.5) ]


messageView : ( Message, User ) -> Element msg
messageView ( message, user ) =
    row [ spacing (em 0.5) ] [ avatar 1 user, text message.body ]


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
        [ userLabel user
        , unreadBadge conv.unread
        , text "❯"
        ]


unreadBadge : Int -> Element msg
unreadBadge count =
    if count > 0 then
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

    else
        Element.none


userLabel : User -> Element msg
userLabel user =
    row
        [ width fill
        , height (eml 3)
        , centerY
        , spacing (em 0.5)
        ]
        [ avatar 2.4 user
        , Element.text user.name
        ]


avatar : Float -> User -> Element msg
avatar size user =
    Element.image
        [ height (eml size)
        , width (eml size)
        , Border.rounded (em <| size / 2)
        , clip
        ]
        { src = user.avatar, description = user.name }
