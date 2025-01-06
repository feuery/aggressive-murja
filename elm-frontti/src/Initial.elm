module Initial exposing (..)

import Article exposing (decodeApply)
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Encode as Json

type alias Initial =
    { initial : Bool }            -- http api can't return bare booleans (as "false"s are interpreted as 404 in the middlewares

type alias InitialFormData =
    { username : String
    , nickname : String
    , password : String 
    , domain : String
    , blog_title : String
    , rss_title : String
    , rss_link : String
    , rss_description : Maybe String
    , rss_lang : String
    , rss_email : String}
    
initialDecoder = succeed Initial
               |> decodeApply (Decode.field "initial" Decode.bool)

initialEncoder i =
    Json.object
        [ ("username", Json.string i.username)
        , ("nickname", Json.string i.nickname)
        , ("password", Json.string i.password)
        , ("domain", Json.string i.domain)
        , ("blog_title", Json.string i.blog_title)
        , ("rss_title", Json.string i.rss_title)
        , ("rss_link", Json.string i.rss_link)
        , ("rss_description", Json.string (Maybe.withDefault i.blog_title i.rss_description))
        , ("rss_lang", Json.string i.rss_lang)
        , ("rss_email", Json.string i.rss_email)]
