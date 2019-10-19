module AppTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (update)
import RemoteData exposing (RemoteData(..), WebData)
import Test exposing (..)


suite : Test
suite =
    describe "initial loading"
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
