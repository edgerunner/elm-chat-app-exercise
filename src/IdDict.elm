module IdDict exposing (Id, IdDict, decoder, toList)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


type alias Id =
    String


type alias IdDict a =
    Dict Id a


decoder : (a -> Id) -> Decoder a -> Decoder (IdDict a)
decoder getId =
    Decode.map (extract getId)
        >> Decode.list
        >> Decode.map Dict.fromList


extract : (a -> x) -> a -> ( x, a )
extract extractor a =
    ( extractor a, a )


toList : IdDict a -> List a
toList =
    Dict.toList >> List.map Tuple.second
