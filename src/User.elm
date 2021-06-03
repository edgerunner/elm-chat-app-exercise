module User exposing (User, Users, avatar, getAll, getMe, name)

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


getAll : (WebData Users -> msg) -> Cmd msg
getAll =
    Api.get "/users" (decoder |> IdDict.decoder id)


getMe : (WebData User -> msg) -> Cmd msg
getMe =
    Api.get "/users/me" decoder


decoder : D.Decoder User
decoder =
    D.map3 Internals
        (D.field "id" Id.decoder)
        (D.field "username" D.string)
        (D.field "avatar_url" D.string)
        |> D.map User


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
