module User exposing (User, get)

import Api
import Json.Decode as D
import RemoteData exposing (WebData)


type alias User =
    { id : String
    , name : String
    , avatar : String
    }


get : (WebData (List User) -> msg) -> Cmd msg
get =
    Api.get "/users" decoder


decoder : D.Decoder (List User)
decoder =
    D.map3 User
        (D.field "id" D.string)
        (D.field "username" D.string)
        (D.field "avatar_url" D.string)
        |> D.list
