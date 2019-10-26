module MessageTests exposing (suite)

import Conversation exposing (Conversation)
import Expect exposing (Expectation)
import Json.Decode as D
import Messages
import RemoteData exposing (RemoteData(..), WebData)
import Task
import Test exposing (..)
import Time


suite : Test
suite =
    describe "Messages"
        [ test "decodes messages" <|
            \_ ->
                let
                    json =
                        """
                    [
                    {"id": "3", "conversation_id": "3", "body": "Hi!", "from_user_id": "1", "created_at": "2016-08-23T10:15:00.670Z"},
                    {"id": "7", "conversation_id": "3", "body": "Waddap!", "from_user_id": "4", "created_at": "2016-08-23T10:14:00.670Z"}
                    ]
                    """
                in
                case D.decodeString Messages.decoder json of
                    Ok messages ->
                        messages
                            |> Expect.equal
                                [ { id = "3"
                                  , conversation = "3"
                                  , body = "Hi!"
                                  , from = "1"
                                  , time = Time.millisToPosix 1471947300670
                                  }
                                , { id = "7"
                                  , conversation = "3"
                                  , body = "Waddap!"
                                  , from = "4"
                                  , time = Time.millisToPosix 1471947240670
                                  }
                                ]

                    Err err ->
                        D.errorToString err
                            |> Expect.fail
        ]
