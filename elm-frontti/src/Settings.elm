module Settings exposing (..)

import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Decode.Extra as Extra
import Json.Encode as Json exposing (..)

type alias Settings =
    { time_format : String
    , blog_title : String
    , recent_post_count : Int}

settingsDecoder = Decode.map3 Settings
                  (Decode.field "time-format" Decode.string)
                  (Decode.field "blog-title" Decode.string)
                  (Decode.field "recent-post-count" Decode.int)
                     
encodeSettings settings =
    object
        [ ( "time-format", string settings.time_format )
        , ( "blog-title", string settings.blog_title)
        , ( "recent-post-count", int settings.recent_post_count)]
