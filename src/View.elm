module View exposing (view)

import Chat exposing (Model, Msg, focusedMessages)
import Conversation exposing (Conversation)
import Dict
import Element exposing (Element, alignTop, centerX, centerY, column, el, fill, height, padding, pointer, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Events as Events
import IdDict exposing (Id)
import Message exposing (Message)
import Styles exposing (blue, em, gray)
import User exposing (User)


breakpoint : Int
breakpoint =
    em 20


view : Model -> Element Msg
view model =
    (if Chat.width model > breakpoint then
        fullView

     else
        Chat.focus model
            |> Maybe.map (always conversationView)
            |> Maybe.withDefault listView
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
        [ fullViewBlock shrink (listView model)
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


listView : Model -> Element Msg
listView model =
    row
        [ width fill
        , height fill
        ]
        [ convList model
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


convList : Model -> Element Msg
convList model =
    column
        [ width fill
        , height shrink
        , alignTop
        ]
        (List.filterMap
            (\conv ->
                let
                    user =
                        Dict.get conv.with (Chat.users model)

                    listing justUser =
                        el
                            ([ Events.onClick (Chat.focusConversationMsg conv)
                             , width fill
                             , pointer
                             ]
                                ++ selectionAttributes conv (Chat.focus model)
                            )
                            (Conversation.listing conv justUser)
                in
                Maybe.map listing user
            )
            (model |> Chat.conversations |> IdDict.toList)
        )


conversationView : Model -> Element Msg
conversationView model =
    el
        [ width fill
        , height fill
        , Events.onClick Chat.blurConversationMsg
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
