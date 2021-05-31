module Id exposing (Id, decoder, toString)

import Json.Decode as Decode exposing (Decoder)


type Id
    = Id String


decoder : Decoder Id
decoder =
    Decode.string
        |> Decode.andThen ensureNotEmpty
        |> Decode.map Id


ensureNotEmpty : String -> Decoder String
ensureNotEmpty possibleId =
    case possibleId of
        "" ->
            Decode.fail "Id cannot be an empty string"

        id ->
            Decode.succeed id


toString : Id -> String
toString (Id id) =
    id
