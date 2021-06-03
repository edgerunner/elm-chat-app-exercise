module Main exposing (main)

import Browser
import Chat
import Conversation exposing (Conversations)
import Element exposing (layout)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import RemoteData exposing (RemoteData(..), WebData)
import Tuple.Trio as Trio
import User exposing (User, Users)
import View


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = AppLoading LoadingModel
    | Chat Chat.Model
    | AppLoadingError String


type alias LoadingModel =
    { users : WebData Users
    , conversations : WebData Conversations
    , me : WebData User
    }


type Msg
    = GotUsers (WebData Users)
    | GotConversations (WebData Conversations)
    | GotMe (WebData User)
    | ChatMsg Chat.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotUsers webData, AppLoading loadingModel ) ->
            dataCheck { loadingModel | users = webData }

        ( GotConversations webData, AppLoading loadingModel ) ->
            dataCheck { loadingModel | conversations = webData }

        ( GotMe webData, AppLoading loadingModel ) ->
            dataCheck { loadingModel | me = webData }

        ( ChatMsg chatMsg, Chat chatModel ) ->
            Chat.update chatMsg chatModel
                |> Tuple.mapBoth Chat (Cmd.map ChatMsg)

        _ ->
            noop model


dataCheck : LoadingModel -> ( Model, Cmd Msg )
dataCheck lModel =
    case
        RemoteData.succeed Trio.intrine
            |> RemoteData.andMap lModel.users
            |> RemoteData.andMap lModel.conversations
            |> RemoteData.andMap lModel.me
    of
        Success ( users, conversations, me ) ->
            Chat.init users conversations me
                |> Tuple.mapBoth Chat (Cmd.map ChatMsg)

        NotAsked ->
            noop <| AppLoading lModel

        Loading ->
            noop <| AppLoading lModel

        Failure _ ->
            noop <| AppLoadingError "Loading error"


view : Model -> Html Msg
view model =
    case model of
        AppLoading lm ->
            loadingView lm

        Chat cm ->
            View.view cm
                |> Element.map ChatMsg
                |> layout []

        AppLoadingError e ->
            loadingErrorView e


loadingView : LoadingModel -> Html Msg
loadingView model =
    Debug.toString model
        |> text


loadingErrorView : String -> Html Msg
loadingErrorView error =
    div [ class "error" ] [ text error ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Chat chatModel ->
            Sub.map ChatMsg (Chat.subscriptions chatModel)

        _ ->
            Sub.none


init : () -> ( Model, Cmd Msg )
init () =
    ( AppLoading (LoadingModel NotAsked NotAsked NotAsked)
    , Cmd.batch
        [ User.getAll GotUsers
        , Conversation.getAll GotConversations
        , User.getMe GotMe
        ]
    )


noop : Model -> ( Model, Cmd msg )
noop model =
    ( model, Cmd.none )
