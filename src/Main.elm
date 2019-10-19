module Main exposing (LoadingModel, Model(..), Msg(..), init, subscriptions, update, view)

import Browser
import Conversation exposing (Conversation)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, src)
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
    | Chat ChatModel
    | AppLoadingError String


type alias LoadingModel =
    { users : WebData (List User)
    , conversations : WebData (List Conversation)
    }


type alias ChatModel =
    { users : Dict String User
    , conversations : List Conversation
    , currentUser : String
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
                Chat (ChatModel (keyById users) conversations "1")

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


chatView : ChatModel -> Html Msg
chatView model =
    div [ class "chat" ]
        [ convList model
        ]


convList : ChatModel -> Html Msg
convList model =
    ul [ class "conversations" ]
        (List.filterMap (convListing (userById model.users)) model.conversations)


convListing : (String -> Maybe User) -> Conversation -> Maybe (Html Msg)
convListing user conv =
    Maybe.map
        (\u -> li [] [ userLabel u, unreadBadge conv.unread ])
        (user conv.with)


userLabel : User -> Html Msg
userLabel user =
    h4 []
        [ img [ src user.avatar ] []
        , text user.name
        ]


unreadBadge : Int -> Html Msg
unreadBadge count =
    case count of
        0 ->
            text ""

        _ ->
            span [ class "unread" ] [ text <| String.fromInt count ]


userById : Dict String User -> String -> Maybe User
userById users id =
    Dict.get id users


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
