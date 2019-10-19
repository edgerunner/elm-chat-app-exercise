module Main exposing (LoadingModel, Model(..), Msg(..), init, subscriptions, update, view)

import Browser
import Chat exposing (chatView)
import Conversation exposing (Conversation)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class)
import RemoteData exposing (RemoteData(..), WebData, append)
import User exposing (User)


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
    { users : WebData (List User)
    , conversations : WebData (List Conversation)
    }


type alias Message =
    { id : Int
    , text : String
    , from : Int
    }


type Msg
    = GotUsers (WebData (List User))
    | GotConversations (WebData (List Conversation))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotUsers webData, AppLoading loadingModel ) ->
            dataCheck { loadingModel | users = webData }

        ( GotConversations webData, AppLoading loadingModel ) ->
            dataCheck { loadingModel | conversations = webData }

        _ ->
            noop model


dataCheck : LoadingModel -> ( Model, Cmd Msg )
dataCheck lModel =
    noop <|
        case append lModel.users lModel.conversations of
            Success ( users, conversations ) ->
                Chat (Chat.Model (keyById users) conversations "1")

            NotAsked ->
                AppLoading lModel

            Loading ->
                AppLoading lModel

            Failure error ->
                AppLoadingError "Loading error"


type alias WithId a =
    { a | id : String }


keyById : List (WithId a) -> Dict String (WithId a)
keyById =
    List.foldl
        (\i -> Dict.insert i.id i)
        Dict.empty


view : Model -> Html Msg
view model =
    case model of
        AppLoading lm ->
            loadingView lm

        Chat cm ->
            chatView cm

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
    Sub.none


init : () -> ( Model, Cmd Msg )
init () =
    ( AppLoading (LoadingModel NotAsked NotAsked)
    , Cmd.batch
        [ User.get GotUsers
        , Conversation.get GotConversations
        ]
    )


noop : Model -> ( Model, Cmd msg )
noop model =
    ( model, Cmd.none )
