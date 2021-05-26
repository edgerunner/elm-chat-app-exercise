module Chat exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Dom
import Browser.Events
import Conversation exposing (Conversation, Conversations)
import Dict
import Element exposing (Element, alignTop, centerX, centerY, column, el, fill, height, padding, pointer, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Events as Events
import IdDict
import Message
import Styles exposing (blue, em, gray)
import Task
import User exposing (Users)


type Model
    = Model ModelRecord


type alias ModelRecord =
    { users : Users
    , conversations : Conversations
    , focus : Maybe Conversation
    , width : Int
    }


type Msg
    = FocusConversation Conversation
    | BlurConversation
    | WindowResize Int Int
    | WindowInitialize Browser.Dom.Viewport
    | GotMessages Conversation


init : Users -> Conversations -> ( Model, Cmd Msg )
init users conversations =
    ( Model
        { users = users
        , conversations = conversations
        , focus = Nothing
        , width = 0
        }
    , Task.perform WindowInitialize Browser.Dom.getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        only transform =
            ( transform model, Cmd.none )
    in
    case msg of
        FocusConversation conv ->
            conv
                |> Conversation.checkAndLoad GotMessages
                |> Tuple.mapFirst
                    (\c ->
                        model
                            |> replaceConversation c
                            >> focusConversation c
                    )

        BlurConversation ->
            only blurConversation

        WindowResize width _ ->
            only <| windowResize width

        WindowInitialize window ->
            only <| windowResize (truncate window.viewport.width)

        GotMessages conversation ->
            only
                (replaceConversation conversation
                    >> focusConversation conversation
                )


replaceConversation : Conversation -> Model -> Model
replaceConversation conversation (Model model) =
    Model
        { model
            | conversations =
                model.conversations
                    |> Dict.update conversation.id (\_ -> Just conversation)
        }


focusConversation : Conversation -> Model -> Model
focusConversation conv (Model model) =
    Model { model | focus = Just conv }


blurConversation : Model -> Model
blurConversation (Model model) =
    Model { model | focus = Nothing }


windowResize : Int -> Model -> Model
windowResize width (Model model) =
    Model { model | width = width }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize WindowResize


breakpoint : Int
breakpoint =
    em 20


view : Model -> Element Msg
view (Model model) =
    (if model.width > breakpoint then
        fullView

     else
        model.focus
            |> Maybe.map (always conversationView)
            |> Maybe.withDefault listView
    )
        (Model model)


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


selectionAttributes : Conversation -> Maybe Conversation -> List (Element.Attribute msg)
selectionAttributes conv focus =
    case focus of
        Just current ->
            if current == conv then
                [ Background.color blue ]

            else
                []

        Nothing ->
            []


convList : Model -> Element Msg
convList (Model model) =
    column
        [ width fill
        , height shrink
        , alignTop
        ]
        (List.filterMap
            (\conv ->
                let
                    user =
                        Dict.get conv.with model.users

                    listing justUser =
                        el
                            ([ Events.onClick (FocusConversation conv)
                             , width fill
                             , pointer
                             ]
                                ++ selectionAttributes conv model.focus
                            )
                            (Conversation.listing conv justUser)
                in
                Maybe.map listing user
            )
            (model.conversations |> IdDict.toList)
        )


conversationView : Model -> Element Msg
conversationView (Model model) =
    let
        convViev =
            case model.focus of
                Just conv ->
                    Message.view conv.messages

                Nothing ->
                    el
                        [ centerX
                        , centerY
                        ]
                        (text "Select conversation")
    in
    el
        [ width fill
        , height fill
        , Events.onClick BlurConversation
        ]
        convViev
