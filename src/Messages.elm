module Messages exposing (Model, Msg, decoder, get)

import Api
import Iso8601
import Json.Decode as D
import RemoteData exposing (WebData)
import Time exposing (Posix)
import User exposing (User)


type alias Message =
    { id : String
    , from : String
    , conversation : String
    , body : String
    , time : Posix
    }


type alias Model =
    WebData Messages


type alias Messages =
    List Message


type alias Id =
    String


type Msg
    = GotMessages Id Model


get : Id -> Cmd Msg
get convId =
    let
        path =
            "/conversations/" ++ convId ++ "/messages"
    in
    Api.get path decoder (GotMessages convId)



{- // sample JSON object
   "id": "7",
   "conversation_id": "3",
   "body": "Waddap!",
   "from_user_id": "4",
   "created_at": "2016-08-23T10:14:00.670Z"
-}


decoder : D.Decoder Messages
decoder =
    D.map5 Message
        (D.field "id" D.string)
        (D.field "from_user_id" D.string)
        (D.field "conversation_id" D.string)
        (D.field "body" D.string)
        (D.field "created_at" Iso8601.decoder)
        |> D.list
