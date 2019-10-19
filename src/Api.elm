module Api exposing (get)

import Http
import Json.Decode as D
import RemoteData exposing (WebData, fromResult)


get : String -> D.Decoder typ -> (WebData typ -> msg) -> Cmd msg
get endpoint decoder msg =
    Http.get
        { url = "http://ui-developer-backend.herokuapp.com/api" ++ endpoint
        , expect =
            Http.expectJson
                (fromResult >> msg)
                decoder
        }
