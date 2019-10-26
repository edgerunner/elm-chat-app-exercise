module AppTests exposing (suite)

import Chat
import Conversation
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.Attributes as Attr
import Main exposing (update)
import Messages
import RemoteData exposing (RemoteData(..), WebData)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (text)
import User


suite : Test
suite =
    describe "Chat app"
        [ describe "initial loading"
            [ test "stays loading when users are delivered first" <|
                \_ ->
                    let
                        msg =
                            Main.GotUsers (Success [])

                        model =
                            Main.LoadingModel NotAsked NotAsked
                                |> Main.AppLoading

                        state =
                            update msg model
                                |> Tuple.first
                    in
                    case state of
                        Main.AppLoading _ ->
                            Expect.pass

                        _ ->
                            Expect.fail "App must stay loading until all components finish"
            , test "stays loading when conversations are delivered first" <|
                \_ ->
                    let
                        msg =
                            Main.GotConversations (Success [])

                        model =
                            Main.LoadingModel NotAsked NotAsked
                                |> Main.AppLoading

                        state =
                            update msg model
                                |> Tuple.first
                    in
                    case state of
                        Main.AppLoading _ ->
                            Expect.pass

                        _ ->
                            Expect.fail "App must stay loading until all components finish"
            , test "switches to chat when both resources are loaded" <|
                \_ ->
                    let
                        msg =
                            Main.GotUsers (Success [])

                        model =
                            Main.LoadingModel NotAsked (Success [])
                                |> Main.AppLoading

                        state =
                            update msg model
                                |> Tuple.first
                    in
                    case state of
                        Main.Chat _ ->
                            Expect.pass

                        _ ->
                            Expect.fail "app must switch to chat when both resources are loaded"
            ]
        , describe "loaded state"
            [ test "must display list of conversations when loading completes" <|
                \_ ->
                    let
                        users =
                            [ User.User "1" "Vehbi" "john.png" ]

                        conversations =
                            [ Conversation.Conversation "1" "1" 1837 (Messages.init "1") ]

                        chat =
                            Chat.init users conversations

                        model =
                            Tuple.mapBoth Main.Chat (Cmd.map Main.ChatMsg) chat
                                |> Tuple.first

                        view =
                            Main.view model
                    in
                    view
                        |> Query.fromHtml
                        |> Query.has
                            [ text "Vehbi"
                            , text "1837"
                            , Selector.attribute (Attr.src "john.png")
                            ]
            ]
        ]
