module User exposing (User, avatar, get, label)

import Api
import Dict exposing (Dict)
import Element exposing (Element, centerY, clip, fill, height, row, spacing, width)
import Element.Border as Border
import Json.Decode as D
import RemoteData exposing (WebData)
import Styles exposing (em, eml)


type alias User =
    { id : String
    , name : String
    , avatar : String
    }


get : (WebData (Dict String User) -> msg) -> Cmd msg
get =
    Api.get "/users" decoder


decoder : D.Decoder (Dict String User)
decoder =
    D.map3 User
        (D.field "id" D.string)
        (D.field "username" D.string)
        (D.field "avatar_url" D.string)
        |> D.map (extract .id)
        |> D.list
        |> D.map Dict.fromList


extract : (a -> x) -> a -> ( x, a )
extract extractor a =
    ( extractor a, a )


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
