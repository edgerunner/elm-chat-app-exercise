module IdDict exposing (IdDict, decoder, get, toList, update)

import Dict exposing (Dict)
import Id exposing (Id)
import Json.Decode as Decode exposing (Decoder)


type IdDict a
    = IdDict (Dict String a)


map : (Dict String a -> Dict String a) -> IdDict a -> IdDict a
map transform =
    peek >> transform >> IdDict


peek : IdDict a -> Dict String a
peek (IdDict dict) =
    dict


decoder : (a -> Id) -> Decoder a -> Decoder (IdDict a)
decoder getId =
    Decode.map (extract getId)
        >> Decode.list
        >> Decode.map Dict.fromList
        >> Decode.map IdDict


extract : (a -> Id) -> a -> ( String, a )
extract extractor a =
    ( extractor a |> Id.toString, a )


toList : IdDict a -> List a
toList =
    peek >> Dict.toList >> List.map Tuple.second


get : Id -> IdDict a -> Maybe a
get id =
    peek >> (Dict.get <| Id.toString id)


update : Id -> (Maybe v -> Maybe v) -> IdDict v -> IdDict v
update id update_ =
    map <| Dict.update (Id.toString id) update_
