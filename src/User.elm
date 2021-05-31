module User exposing (User, Users, avatar, get, name)

import Api
import Id exposing (Id)
import IdDict exposing (IdDict)
import Json.Decode as D
import RemoteData exposing (WebData)


type User
    = User Internals


type alias Internals =
    { id : Id
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
    D.map3 Internals
        (D.field "id" Id.decoder)
        (D.field "username" D.string)
        (D.field "avatar_url" D.string)
        |> D.map User
        |> IdDict.decoder id


internals : (Internals -> a) -> User -> a
internals extract (User internals_) =
    extract internals_


id : User -> Id
id =
    internals .id


name : User -> String
name =
    internals .name


avatar : User -> String
avatar =
    internals .avatar
