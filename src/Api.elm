module Api exposing (get)

import Http
import Json.Decode as D
import RemoteData exposing (WebData, fromResult)


get : String -> D.Decoder typ -> (WebData typ -> msg) -> Cmd msg
get endpoint decoder msg =
    Http.get
        { url = "/api" ++ endpoint ++ ".json"
        , expect =
            Http.expectJson
                (fromResult >> msg)
                decoder
        }
