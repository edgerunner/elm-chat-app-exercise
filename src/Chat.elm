module Chat exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Dom
import Browser.Events
import Conversation exposing (Conversation, convListing)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Messages exposing (Messages)
import RemoteData exposing (WebData)
import Styles exposing (em, eml, gray)
import Task
import User exposing (User, userLabel)


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
    | GotMessages String (WebData Messages)


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
            , Messages.get conv.id (GotMessages conv.id)
            )

        BlurConversation ->
            ( blurConversation model, Cmd.none )

        WindowResize width _ ->
            ( windowResize width model, Cmd.none )

        WindowInitialize window ->
            ( windowResize (truncate window.viewport.width) model, Cmd.none )

        GotMessages convId messages ->
            ( gotMessages convId messages model, Cmd.none )


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


gotMessages : String -> WebData Messages -> Model -> Model
gotMessages convId messages (Model model) =
    let
        conversations =
            List.map
                (\conv ->
                    if conv.id == convId then
                        { conv | messages = messages }

                    else
                        conv
                )
                model.conversations

        focus =
            case model.focus of
                ConversationView conv ->
                    ConversationView { conv | messages = messages }

                FullView (Just conv) ->
                    FullView (Just { conv | messages = messages })

                other ->
                    other
    in
    Model { model | conversations = conversations, focus = focus }


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
        [ listView model
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
                            [ Events.onClick (FocusConversation conv)
                            , width fill
                            ]
                            (convListing conv justUser)
                in
                Maybe.map listing user
            )
            model.conversations
        )


conversationView : Model -> Element Msg
conversationView (Model model) =
    paragraph
        [ width fill
        , height fill
        , Background.color gray
        , Events.onClick BlurConversation
        ]
        [ text (Debug.toString model.focus) ]
