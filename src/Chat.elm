module Chat exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Dom
import Browser.Events
import Conversation exposing (Conversation)
import Dict exposing (Dict)
import Element exposing (Element, alignTop, centerX, centerY, column, el, fill, height, padding, pointer, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Events as Events
import Styles exposing (blue, em, gray)
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
    | GotMessages Conversation


type Focus
    = FullView (Maybe Conversation)
    | ListView
    | ConversationView Conversation


init : Dict String User -> List Conversation -> ( Model, Cmd Msg )
init users conversations =
    ( Model
        { users = users
        , conversations = conversations
        , focus = ListView
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
            ( focusConversation conv model
            , Conversation.getMessages GotMessages conv
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
    let
        conversations =
            model.conversations
                |> List.map
                    (\conv ->
                        if conv.id == conversation.id then
                            conversation

                        else
                            conv
                    )
    in
    Model { model | conversations = conversations }


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


selectionAttributes : Conversation -> Focus -> List (Element.Attribute msg)
selectionAttributes conv focus =
    case focus of
        FullView (Just current) ->
            if current == conv then
                [ Background.color blue ]

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
                             , pointer
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
