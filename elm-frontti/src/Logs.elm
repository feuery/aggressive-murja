module Logs exposing (..)

import Article exposing (decodeApply)
import Json.Decode as Decode exposing (Decoder, succeed)

type alias Log =
    { row : String              -- let's keep them as just dumb lines for now and add smarter analytics later...
          }


rowDecoder = Decode.field "row" Decode.string
decoder = Decode.succeed Log |> decodeApply rowDecoder             
