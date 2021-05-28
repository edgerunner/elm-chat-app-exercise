module User exposing (User, Users, get, unknown)

import Api
import IdDict exposing (IdDict)
import Json.Decode as D
import RemoteData exposing (WebData)


type alias User =
    { id : String
    , name : String
    , avatar : String
    }


type alias Users =
    IdDict User


get : (WebData Users -> msg) -> Cmd msg
get =
    Api.get "/users" decoder


decoder : D.Decoder Users
decoder =
    D.map3 User
        (D.field "id" D.string)
        (D.field "username" D.string)
        (D.field "avatar_url" D.string)
        |> IdDict.decoder


unknown : User
unknown =
    { id = ""
    , name = "Unknown user"
    , avatar = ""
    }
