module IdDict exposing (IdDict, decoder, get, toList, update)

import Dict exposing (Dict)
import Id exposing (Id)
import Json.Decode as Decode exposing (Decoder)


type alias IdDict a =
    Dict String a


decoder : (a -> Id) -> Decoder a -> Decoder (IdDict a)
decoder getId =
    Decode.map (extract getId)
        >> Decode.list
        >> Decode.map Dict.fromList


extract : (a -> Id) -> a -> ( String, a )
extract extractor a =
    ( extractor a |> Id.toString, a )


toList : IdDict a -> List a
toList =
    Dict.toList >> List.map Tuple.second


get : Id -> IdDict a -> Maybe a
get id =
    Dict.get <| Id.toString id


update : Id -> (Maybe v -> Maybe v) -> IdDict v -> IdDict v
update id update_ =
    Dict.update (Id.toString id) update_
