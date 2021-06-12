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
            Message.asClusters dict
                |> populateClusters
                    (Chat.me model)
                    (Chat.user (Conversation.with conv) model)
                |> clustersView

        Failure _ ->
            blob "Error loading messages"


clustersView : List { first : Message, rest : List Message, sender : Maybe User } -> Element msg
clustersView =
    List.map clusterView >> column [ spacing (em 1) ]


clusterView : { first : Message, rest : List Message, sender : Maybe User } -> Element msg
clusterView { first, rest, sender } =
    row [ spacing (em 0.5) ]
        [ Maybe.map (avatar 2) sender
            |> Maybe.withDefault Element.none
        , messagesView (first :: rest)
        ]


populateClusters :
    User
    -> Maybe User
    -> List { first : Message, rest : List Message }
    -> List { first : Message, rest : List Message, sender : Maybe User }
populateClusters me otherUser =
    let
        sender message =
            if Message.incoming message then
                otherUser

            else
                Just me
    in
    List.map (\{ first, rest } -> { sender = sender first, first = first, rest = rest })


messagesView : List Message -> Element msg
messagesView =
    List.map messageView >> column [ spacing (em 0.5), alignTop ]


messageView : Message -> Element msg
messageView =
    text << Message.body


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
        , alignTop
        ]
        { src = User.avatar user, description = User.name user }
