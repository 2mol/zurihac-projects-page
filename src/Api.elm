port module Api exposing (idsDecoder)

import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, string)


port storeCache : Maybe Value -> Cmd msg


port onStoreChange : (Value -> msg) -> Sub msg


idsDecoder : Decoder (List Int)
idsDecoder =
    Decode.list Decode.int
