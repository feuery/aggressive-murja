module Feeds exposing (..)

import Json.Encode as Json exposing (..)
import Json.Encode.Extra exposing (..)
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Decode.Extra as Extra

import UUID exposing (UUID)

import Article exposing (decodeApply)
import Creator exposing (Creator)

type alias Feed =
    { id: UUID
    , name: String 
    , url: String
    , creator: Creator}

type alias NewFeed =
    { name: String 
    , url: String}

creatorDecoder = Decode.field "creator" Creator.creatorDecoder
urlDecoder = Decode.field "url" Decode.string
nameDecoder = (Decode.field "name" Decode.string)
idDecoder = (Decode.field "id" UUID.jsonDecoder)

feedDecoder: Decoder Feed
feedDecoder =
    Decode.succeed Feed
        |> decodeApply idDecoder
        |> decodeApply nameDecoder
        |> decodeApply urlDecoder
        |> decodeApply creatorDecoder

newFeedDecoder: Decoder NewFeed
newFeedDecoder =
    Decode.succeed NewFeed
        |> decodeApply nameDecoder
        |> decodeApply urlDecoder

newFeedEncoder o =
    Json.object
        [ ("name", Json.string o.name)
        , ("url", Json.string o.url) ]
        
