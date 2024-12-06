module Logs exposing (..)

import Article exposing (decodeApply)
import Json.Decode as Decode exposing (Decoder, succeed)
import Regex exposing (Regex)

type alias Log =
    { row : String              -- let's keep them as just dumb lines for now and add smarter analytics later...
          }

type alias Group =
    { name: String
    , alarmy: Bool 
    , members: List Log}

type alias ParsedGroup =
    { name: String
    , regex: Regex
    , alarmy: Bool 
    , members: List Log}

regex_to_parsedgroup str =
    case Regex.fromString str of
        Just re -> Just <| ParsedGroup str re False []
        Nothing -> Nothing
                   
str_to_group str =
    Group str False []

parsedGroup_to_group pg =
    Group pg.name pg.alarmy pg.members
    
rowDecoder = Decode.field "row" Decode.string
decoder = Decode.succeed Log |> decodeApply rowDecoder             
