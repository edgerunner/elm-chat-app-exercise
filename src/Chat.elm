module Chat exposing
    ( Model
    , Msg
    , conversation
    , conversations
    , focus
    , focusedConversation
    , focusedMessages
    , init
    , messageFrom
    , msg
    , subscriptions
    , update
    , user
    , users
    , width
    )

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


msg :
    { focusConversation : Conversation -> Msg
    , blurConversation : Msg
    }
msg =
    { focusConversation = FocusConversation
    , blurConversation = BlurConversation
    }


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
update msg_ model =
    let
        only transform =
            ( transform model, Cmd.none )
    in
    case msg_ of
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

        GotMessages conversation_ ->
            only <| replaceAndFocusConversation conversation_


replaceAndFocusConversation : Conversation -> Model -> Model
replaceAndFocusConversation conversation_ =
    replaceConversation conversation_
        >> focusConversation conversation_.id


replaceConversation : Conversation -> Model -> Model
replaceConversation conversation_ (Model model) =
    Model
        { model
            | conversations =
                model.conversations
                    |> Dict.update conversation_.id (\_ -> Just conversation_)
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



-- SELECTORS


focusedConversation : Model -> Maybe Conversation
focusedConversation model =
    model
        |> peek .focus
        |> Maybe.map Dict.get
        |> Maybe.andThen ((|>) (peek .conversations model))


focusedMessages : Model -> Maybe (List Message)
focusedMessages model =
    model
        |> focusedConversation
        |> Maybe.map .messages
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map IdDict.toList


messageFrom : Message -> Model -> User
messageFrom message =
    user message.from
        >> Maybe.withDefault User.unknown


users : Model -> List User
users =
    peek .users >> IdDict.toList


user : Id -> Model -> Maybe User
user id =
    peek .users >> Dict.get id


conversations : Model -> List Conversation
conversations =
    peek .conversations >> IdDict.toList


conversation : Id -> Model -> Maybe Conversation
conversation id =
    peek .conversations >> Dict.get id


width : Model -> Int
width =
    peek .width


focus : Model -> Maybe Id
focus =
    peek .focus


peek : (ModelRecord -> a) -> Model -> a
peek map_ (Model model) =
    map_ model
