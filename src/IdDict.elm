module IdDict exposing (Id, IdDict, decoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


type alias Id =
    String


type alias IdDict a =
    Dict Id a


decoder : Decoder { a | id : Id } -> Decoder (IdDict { a | id : Id })
decoder =
    Decode.map (extract .id)
        >> Decode.list
        >> Decode.map Dict.fromList


extract : (a -> x) -> a -> ( x, a )
extract extractor a =
    ( extractor a, a )
