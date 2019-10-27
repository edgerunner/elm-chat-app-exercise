module Messages exposing (Model, Msg, decoder, get, init, update, view)

import Api
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Iso8601
import Json.Decode as D
import RemoteData exposing (WebData)
import Styles exposing (..)
import Time exposing (Posix)
import User exposing (User)


type alias Message =
    { id : String
    , from : String
    , conversation : String
    , body : String
    , time : Posix
    }


type Model
    = Model (WebData Messages)


init : Model
init =
    Model RemoteData.NotAsked


type alias Messages =
    List Message


type alias Id =
    String


type Msg
    = GotMessages Model


get : Id -> Cmd Msg
get convId =
    let
        path =
            "/conversations/" ++ convId ++ "/messages"
    in
    Api.get path decoder (GotMessages << Model)



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



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMessages newModel ->
            ( newModel, Cmd.none )



-- VIEW


view : Model -> Element msg
view (Model model) =
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
    column []
        (List.map
            listing
            messages
        )


listing : Message -> Element msg
listing message =
    el [] (text message.body)
