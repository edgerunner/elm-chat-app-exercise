module User exposing (User, get, userLabel)

import Api
import Element exposing (..)
import Element.Border as Border
import Json.Decode as D
import RemoteData exposing (WebData)
import Styles exposing (em, eml)


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


userLabel : User -> Element msg
userLabel user =
    row
        [ width fill
        , height (eml 3)
        , centerY
        , spacing (em 0.5)
        ]
        [ Element.image
            [ height (eml 2.4)
            , width (eml 2.4)
            , Border.rounded (em 1.2)
            , clip
            ]
            { src = user.avatar, description = user.name }
        , Element.text user.name
        ]
