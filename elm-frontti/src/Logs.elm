module Logs exposing (..)

import Article exposing (decodeApply)
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Encode as Json exposing (..)
import Regex exposing (Regex)

type alias Log =
    { row : String              -- let's keep them as just dumb lines for now and add smarter analytics later...
          }

type alias Group =
    { name: String
    , alarmy: Bool
    , sound_alarm: Bool               
    , members: List Log}

type alias ParsedGroup =
    { name: String
    , regex: Regex
    , alarmy: Bool
    , sound_alarm: Bool 
    , members: List Log}

regex_to_parsedgroup str =
    case Regex.fromString str of
        Just re -> Just <| ParsedGroup str re False False []
        Nothing -> Nothing
                   
str_to_group str =
    Group str False False []

parsedGroup_to_group pg =
    Group pg.name pg.alarmy pg.members
    
rowDecoder = Decode.field "row" Decode.string
decoder = Decode.succeed Log |> decodeApply rowDecoder             

groupEncoder group =
    object
        [ ( "name", string group.name)
        , ( "alarmy", bool group.alarmy)]

groupsEncoder groups =
    list groupEncoder groups

nameDecoder = Decode.field "name" Decode.string
alarmyDecoder = Decode.field "alarmy" Decode.bool
sound_alarm_decoder = Decode.field "sound-alarm" Decode.bool

groupDecoder = Decode.succeed Group
               |> decodeApply nameDecoder
               |> decodeApply alarmyDecoder
               |> decodeApply sound_alarm_decoder
               |> decodeApply (Decode.succeed [])

groupsDecoder = Decode.list groupDecoder
