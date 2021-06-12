module View exposing (avatar, userLabel, view)

import Chat exposing (Model, Msg)
import Conversation exposing (Conversation)
import Element exposing (Element, alignTop, centerX, centerY, clip, column, el, fill, height, minimum, padding, paddingXY, pointer, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Id exposing (Id)
import Message exposing (Message)
import RemoteData exposing (RemoteData(..))
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
            |> Maybe.map ((==) (Conversation.id conv))
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
                    ([ Events.onClick (Chat.msg.conversationSelected conv)
                     , width fill
                     , pointer
                     ]
                        ++ selectionAttributes conv (Chat.focus model)
                    )
                    (Chat.user (Conversation.with conv) model
                        |> Maybe.map (convListing conv)
                        |> Maybe.withDefault (text "Unknown user")
                    )
            )
            (Chat.conversations model)
        )


conversationView : Model -> Element Msg
conversationView model =
    el
        [ width fill
        , height fill
        , Events.onClick Chat.msg.conversationDeselected
        ]
        (Chat.focusedConversation model
            |> Maybe.map (conversationStateView model)
            |> Maybe.withDefault (blob "Select conversation")
        )


conversationStateView : Model -> Conversation -> Element msg
conversationStateView model conv =
    case Conversation.messages conv of
        NotAsked ->
            blob "A moment please…"

        Loading ->
            blob "Loading…"

        Success dict ->
            Message.asList dict
                |> List.map
                    (\message ->
                        let
                            user =
                                if Message.incoming message then
                                    Chat.user (Conversation.with conv) model

                                else
                                    Chat.me model |> Just
                        in
                        ( message, user )
                    )
                |> messagesView

        Failure _ ->
            blob "Error loading messages"


messagesView : List ( Message, Maybe User ) -> Element msg
messagesView =
    List.map messageView >> column [ padding (em 1), spacing (em 0.5) ]


messageView : ( Message, Maybe User ) -> Element msg
messageView ( message, maybeUser ) =
    case maybeUser of
        Just user ->
            row [ spacing (em 0.5) ] [ avatar 1 user, text <| Message.body message ]

        Nothing ->
            row [ spacing (em 0.5) ] [ text "?", text <| Message.body message ]


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
        , unreadBadge (Conversation.unread conv)
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
        , Element.text (User.name user)
        ]


avatar : Float -> User -> Element msg
avatar size user =
    Element.image
        [ height (eml size)
        , width (eml size)
        , Border.rounded (em <| size / 2)
        , clip
        ]
        { src = User.avatar user, description = User.name user }
