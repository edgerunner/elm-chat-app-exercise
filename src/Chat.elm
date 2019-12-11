module Chat exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Dom
import Browser.Events
import Conversation exposing (Conversation)
import Dict exposing (Dict)
import Element exposing (Element, alignTop, centerX, centerY, column, el, fill, height, maximum, none, row, shrink, text, width)
import Element.Background as Background
import Element.Events as Events
import Styles exposing (em, eml, gray)
import Task
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
    | WindowResize Int Int
    | WindowInitialize Browser.Dom.Viewport
    | ConversationMsg Conversation.Msg


type Focus
    = FullView (Maybe Conversation)
    | ListView
    | ConversationView Conversation


init : List User -> List Conversation -> ( Model, Cmd Msg )
init users conversations =
    ( Model
        { users = List.foldl (\user -> Dict.insert user.id user) Dict.empty users
        , conversations = conversations
        , focus = ListView
        }
    , Task.perform WindowInitialize Browser.Dom.getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FocusConversation conv ->
            ( focusConversation conv model
            , Conversation.getMessages conv
                |> Cmd.map ConversationMsg
            )

        BlurConversation ->
            ( blurConversation model, Cmd.none )

        WindowResize width _ ->
            ( windowResize width model, Cmd.none )

        WindowInitialize window ->
            ( windowResize (truncate window.viewport.width) model, Cmd.none )

        ConversationMsg cMsg ->
            conversationMsg cMsg model


focusConversation : Conversation -> Model -> Model
focusConversation conv model =
    case map .focus model of
        ListView ->
            updateFocus model (ConversationView conv)

        FullView _ ->
            updateFocus model (FullView (Just conv))

        _ ->
            model


blurConversation : Model -> Model
blurConversation model =
    case map .focus model of
        ConversationView _ ->
            updateFocus model ListView

        _ ->
            model


windowResize : Int -> Model -> Model
windowResize width model =
    let
        breakpoint =
            em 20

        wide =
            width > breakpoint

        focusOn =
            updateFocus model
    in
    case ( map .focus model, wide ) of
        ( ConversationView conv, True ) ->
            focusOn (FullView (Just conv))

        ( ListView, True ) ->
            focusOn (FullView Nothing)

        ( FullView Nothing, False ) ->
            focusOn ListView

        ( FullView (Just conv), False ) ->
            focusOn (ConversationView conv)

        _ ->
            model


updateFocus : Model -> Focus -> Model
updateFocus (Model model) focus =
    Model { model | focus = focus }


conversationMsg : Conversation.Msg -> Model -> ( Model, Cmd Msg )
conversationMsg msg (Model model) =
    let
        updateThis =
            Conversation.update msg

        ( conversations, cmd ) =
            updateThis model.conversations

        focus =
            case model.focus of
                FullView (Just conv) ->
                    updateThis [ conv ]
                        |> Tuple.first
                        |> List.head
                        |> FullView

                ConversationView conv ->
                    updateThis [ conv ]
                        |> Tuple.first
                        |> List.head
                        |> Maybe.withDefault conv
                        |> ConversationView

                _ ->
                    model.focus
    in
    ( Model { model | conversations = conversations, focus = focus }, Cmd.map ConversationMsg cmd )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize WindowResize


view : Model -> Element Msg
view model =
    case map .focus model of
        ListView ->
            listView model

        ConversationView _ ->
            conversationView model

        FullView _ ->
            fullView model


fullView : Model -> Element Msg
fullView model =
    row
        [ width fill
        , height fill
        ]
        [ el [ width (maximum (em 12) fill), alignTop ] (listView model)
        , el [ width (eml 1), height fill, Background.color gray ] none
        , conversationView model
        ]


listView : Model -> Element Msg
listView model =
    row
        [ width fill
        , height fill
        ]
        [ convList model
        ]


selectionAttributes : Conversation -> Focus -> List (Element.Attribute msg)
selectionAttributes conv focus =
    case focus of
        FullView (Just current) ->
            if current == conv then
                [ Background.color gray ]

            else
                []

        _ ->
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
                             ]
                                ++ selectionAttributes conv model.focus
                            )
                            (Conversation.listing conv justUser)
                in
                Maybe.map listing user
            )
            model.conversations
        )


conversationView : Model -> Element Msg
conversationView (Model model) =
    let
        convViev =
            case model.focus of
                FullView (Just conv) ->
                    Conversation.view conv

                ConversationView conv ->
                    Conversation.view conv

                _ ->
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
