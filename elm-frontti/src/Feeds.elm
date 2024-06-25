module Feeds exposing (..)

import Json.Encode as Json exposing (..)
import Json.Encode.Extra exposing (..)
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Decode.Extra as Extra
import Time

import UUID exposing (UUID)

import Article exposing (decodeApply)
import Creator exposing (Creator)

    
type alias Item =
    { id: UUID
    , fetched: Time.Posix 
    , title: String
    , description: String 
    , link: String
    , author: String
    , pubdate: Time.Posix
    , is_read: Bool
    -- parent id that's required for ui to function correctly 
    , feed_id: Maybe UUID} 

type alias Feed =
    { id: UUID
    , name: String 
    , url: String
    , creator: Creator
    , items: List Item}

type alias NewFeed =
    { name: String 
    , url: String}

type alias FeedMetadata =
    { last_update_timestamps: List String}

-- Article conversion
itemToArticle item =
    let creator = Creator "" item.author "" in 
    Article.Article creator [] item.description Nothing item.title -1 Nothing (Just 1) (Just item.pubdate) False False [] (Article.Rss item.link)

-- metadata decoder

metadataDecoder = Decode.succeed FeedMetadata
                |> decodeApply (Decode.field "last-update-timestamps" (Decode.list Decode.string))
        
-- feedDecoder

creatorDecoder = Decode.field "creator" Creator.creatorDecoder
urlDecoder = Decode.field "url" Decode.string
nameDecoder = (Decode.field "name" Decode.string)
idDecoder = (Decode.field "id" UUID.jsonDecoder)

-- itemDecoder
fetchedDecoder = Decode.field "fetched" Extra.iso8601
titleDecoder = Decode.field "title" Decode.string
descriptionDecoder = Decode.field "description" Decode.string
linkDecoder = Decode.field "link" Decode.string
authorDecoder = Decode.field "author" Decode.string
pubdateDecoder = Decode.field "pubdate" Extra.iso8601
is_readDecoder = Decode.field "is_read" Decode.bool

itemDecoder =
    Decode.succeed Item
        |> decodeApply idDecoder
        |> decodeApply fetchedDecoder
        |> decodeApply titleDecoder
        |> decodeApply descriptionDecoder
        |> decodeApply linkDecoder
        |> decodeApply authorDecoder
        |> decodeApply pubdateDecoder
        |> decodeApply is_readDecoder
        |> decodeApply (Decode.succeed Nothing)

itemsDecoder =
    Decode.field "items" (Decode.list itemDecoder)
           
feedDecoder: Decoder Feed
feedDecoder =
    Decode.succeed Feed
        |> decodeApply idDecoder
        |> decodeApply nameDecoder
        |> decodeApply urlDecoder
        |> decodeApply creatorDecoder
        |> decodeApply itemsDecoder

newFeedDecoder: Decoder NewFeed
newFeedDecoder =
    Decode.succeed NewFeed
        |> decodeApply nameDecoder
        |> decodeApply urlDecoder

newFeedEncoder o =
    Json.object
        [ ("name", Json.string o.name)
        , ("url", Json.string o.url) ]
        
