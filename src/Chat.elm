module Chat exposing (Model, Msg, blurConversationMsg, conversations, focus, focusConversationMsg, focusedConversation, focusedMessages, init, subscriptions, update, users, width)

import Browser.Dom
import Browser.Events
import Conversation exposing (Conversation, Conversations)
import Dict
import IdDict exposing (Id)
import Message exposing (Message)
import RemoteData
import Task
import User exposing (User, Users)


type Model
    = Model ModelRecord


type alias ModelRecord =
    { users : Users
    , conversations : Conversations
    , focus : Maybe Id
    , width : Int
    }


type Msg
    = FocusConversation Conversation
    | BlurConversation
    | WindowResize Int Int
    | WindowInitialize Browser.Dom.Viewport
    | GotMessages Conversation


focusConversationMsg : Conversation -> Msg
focusConversationMsg =
    FocusConversation


blurConversationMsg : Msg
blurConversationMsg =
    BlurConversation


init : Users -> Conversations -> ( Model, Cmd Msg )
init users_ conversations_ =
    ( Model
        { users = users_
        , conversations = conversations_
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
                    (\c -> replaceAndFocusConversation c model)

        BlurConversation ->
            only blurConversation

        WindowResize width_ _ ->
            only <| windowResize width_

        WindowInitialize window ->
            only <| windowResize (truncate window.viewport.width)

        GotMessages conversation ->
            only <| replaceAndFocusConversation conversation


replaceAndFocusConversation : Conversation -> Model -> Model
replaceAndFocusConversation conversation =
    replaceConversation conversation
        >> focusConversation conversation.id


replaceConversation : Conversation -> Model -> Model
replaceConversation conversation (Model model) =
    Model
        { model
            | conversations =
                model.conversations
                    |> Dict.update conversation.id (\_ -> Just conversation)
        }


focusConversation : Id -> Model -> Model
focusConversation convId (Model model) =
    Model { model | focus = model.conversations |> Dict.get convId |> Maybe.map .id }


blurConversation : Model -> Model
blurConversation (Model model) =
    Model { model | focus = Nothing }


windowResize : Int -> Model -> Model
windowResize width_ (Model model) =
    Model { model | width = width_ }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize WindowResize


focusedConversation : Model -> Maybe Conversation
focusedConversation model =
    model
        |> peek .focus
        |> Maybe.map Dict.get
        |> Maybe.andThen ((|>) (peek .conversations model))


focusedMessages : Model -> Maybe (List ( Message, User ))
focusedMessages model =
    model
        |> focusedConversation
        |> Maybe.map .messages
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map
            (IdDict.toList
                >> List.filterMap
                    (\message ->
                        Dict.get message.from (users model)
                            |> Maybe.map (Tuple.pair message)
                    )
            )


users : Model -> Users
users =
    peek .users


width : Model -> Int
width =
    peek .width


focus : Model -> Maybe Id
focus =
    peek .focus


conversations : Model -> Conversations
conversations =
    peek .conversations


peek : (ModelRecord -> a) -> Model -> a
peek map_ (Model model) =
    map_ model
