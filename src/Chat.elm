module Chat exposing
    ( Model
    , Msg
    , conversation
    , conversations
    , focus
    , focusedConversation
    , focusedMessages
    , init
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
import Id exposing (Id)
import IdDict
import Message exposing (Message)
import RemoteData exposing (WebData)
import Task
import User exposing (User, Users)


type Model
    = Model Internals


type alias Internals =
    { users : Users
    , conversations : Conversations
    , me : User
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


init : Users -> Conversations -> User -> ( Model, Cmd Msg )
init users_ conversations_ me_ =
    ( Model
        { users = users_
        , conversations = conversations_
        , me = me_
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
        >> focusConversation (Conversation.id conversation_)


replaceConversation : Conversation -> Model -> Model
replaceConversation conversation_ (Model model) =
    Model
        { model
            | conversations =
                model.conversations
                    |> Conversation.update conversation_
        }


focusConversation : Id -> Model -> Model
focusConversation convId (Model model) =
    Model { model | focus = model.conversations |> IdDict.get convId |> Maybe.map Conversation.id }


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
        |> internals .focus
        |> Maybe.map IdDict.get
        |> Maybe.andThen ((|>) (internals .conversations model))


focusedMessages : Model -> Maybe (WebData (List Message))
focusedMessages model =
    model
        |> focusedConversation
        |> Maybe.map Conversation.messages
        |> Maybe.map (RemoteData.map IdDict.toList)


users : Model -> List User
users =
    internals .users >> IdDict.toList


user : Id -> Model -> Maybe User
user id =
    internals .users >> IdDict.get id


conversations : Model -> List Conversation
conversations =
    internals .conversations >> IdDict.toList


conversation : Id -> Model -> Maybe Conversation
conversation id =
    internals .conversations >> IdDict.get id


me : Model -> User
me =
    internals .me


width : Model -> Int
width =
    internals .width


focus : Model -> Maybe Id
focus =
    internals .focus


internals : (Internals -> a) -> Model -> a
internals extract (Model model) =
    extract model
