module Message exposing (Messages, Model, decoder, get, init, view)

import Api
import Element exposing (Element, centerX, centerY, column, el, padding, spacing, text)
import Element.Font as Font
import IdDict exposing (IdDict)
import Iso8601
import Json.Decode as D
import RemoteData exposing (WebData)
import Styles exposing (em, red)
import Time exposing (Posix)


type alias Message =
    { id : Id
    , from : Id
    , conversation : Id
    , body : String
    , time : Posix
    }


type alias Model =
    WebData Messages


init : Model
init =
    RemoteData.NotAsked


type alias Messages =
    IdDict Message


type alias Id =
    String


get : Id -> Cmd Model
get convId =
    let
        path =
            "/conversations/" ++ convId ++ "/messages"
    in
    Api.get path decoder identity


decoder : D.Decoder Messages
decoder =
    D.map5 Message
        (D.field "id" D.string)
        (D.field "from_user_id" D.string)
        (D.field "conversation_id" D.string)
        (D.field "body" D.string)
        (D.field "created_at" Iso8601.decoder)
        |> IdDict.decoder



-- VIEW


view : Model -> Element msg
view model =
    case model of
        RemoteData.Success messages ->
            list messages

        RemoteData.Failure error ->
            el
                [ Font.color red
                , padding (em 1)
                , centerX
                , centerY
                ]
                (text <| Debug.toString error)

        _ ->
            el
                [ centerX
                , centerY
                ]
                (text "Loading")


list : Messages -> Element msg
list messages =
    column [ padding <| em 1, spacing <| em 0.5 ]
        (messages
            |> IdDict.toList
            |> List.map listing
        )


listing : Message -> Element msg
listing message =
    el [] (text message.body)
