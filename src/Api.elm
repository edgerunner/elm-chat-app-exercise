module Api exposing (get)

import Http
import Json.Decode as D
import RemoteData exposing (WebData)


get : String -> D.Decoder typ -> (WebData typ -> msg) -> Cmd msg
get endpoint decoder msg =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-API-Key" "872a6cc0" ]
        , url = "https://my.api.mockaroo.com" ++ endpoint ++ ".json"
        , body = Http.emptyBody
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> msg)
                decoder
        , timeout = Nothing
        , tracker = Nothing
        }
