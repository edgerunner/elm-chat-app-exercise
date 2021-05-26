module User exposing (User, Users, avatar, get, label)

import Api
import Element exposing (Element, centerY, clip, fill, height, row, spacing, width)
import Element.Border as Border
import IdDict exposing (IdDict)
import Json.Decode as D
import RemoteData exposing (WebData)
import Styles exposing (em, eml)


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


label : User -> Element msg
label user =
    row
        [ width fill
        , height (eml 3)
        , centerY
        , spacing (em 0.5)
        ]
        [ avatar 2.4 user
        , Element.text user.name
        ]


avatar : Float -> User -> Element msg
avatar size user =
    Element.image
        [ height (eml size)
        , width (eml size)
        , Border.rounded (em <| size / 2)
        , clip
        ]
        { src = user.avatar, description = user.name }
