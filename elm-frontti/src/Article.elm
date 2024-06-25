module Article exposing (..)

import Creator exposing (encode)

import DateTime exposing (DateTime)
import Json.Encode as Json exposing (..)
import Json.Encode.Extra exposing (..)
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Decode.Extra as Extra
import Time

-- {
--   "tags": [],
--   "creator": {
--     "username": "feuer",
--     "nickname": "Feuer",
--     "img_location": "https://feuerx.net/etc/feuer.jpeg"
--   },
--   "content": "<p>Tämä on testi posti :D</p>\n\n<p>Uusi paragraaaaaaaafffi</p>",
--   "comments": [],
--   "amount-of-comments": 0,
--   "title": "Testi Posti",
--   "id": 1,
--   "versions": [],
--   "version": null,
--   "created_at": "2020-10-16T07:52:59Z",
--   "previosly": [{"id": 666,
--                  "title": "Titteli"}]
-- }

import Creator exposing (Creator, creatorDecoder)

decodeApply : Decode.Decoder a -> Decode.Decoder (a -> b) -> Decode.Decoder b
decodeApply value partial =
    Decode.andThen (\p -> Decode.map p value) partial

type ArticleSource
    = Murja
    | Rss
      String -- <-link 
      
type alias PreviousArticle =
    { id: Int
    , title: String}

type alias Article =
    { creator : Creator
    , tags : List String
    , content : String
    -- TODO make a comment type
    , comments : Maybe (List String)
    -- , amount_of_comments : Int
    , title : String
    , id : Int
    , versions: Maybe (List Int)
    , version : Maybe Int
    , created_at: Maybe Time.Posix
    , hidden : Bool
    , unlisted : Bool
    , previously: List PreviousArticle
    , source: ArticleSource}

-- encoder

encodePreviously prev =
    object
      [ ( "id", int prev.id)
      , ( "title", string prev.title)]

encode : Article -> Json.Value
encode article =
    object
        [ ( "creator", Creator.encode article.creator )
        , ( "tags", list string article.tags)
        , ( "content", string article.content)
        , ( "comments", (list string (case article.comments of
                                         Just comments -> comments
                                         Nothing -> [])))
        , ( "title", string article.title)
        , ( "id", int article.id)
        , ( "version", (maybe int) article.version)
        , ( "created_at", (maybe iso8601) article.created_at)
        , ( "hidden", bool article.hidden)
        , ( "unlisted", bool article.unlisted)
        , ( "previously", list encodePreviously article.previously)
        ]


-- decoder

    
tagsDecoder = Decode.field "tags" (Decode.list Decode.string)
contentDecoder = Decode.field "content" Decode.string
commentsDecoder = Decode.maybe (Decode.field "comments" (Decode.list Decode.string))
-- amount_of_commentsDecoder = Decode.field "amount-of-comments" Decode.int                  
titleDecoder = Decode.field "title" Decode.string
idDecoder = Decode.field "id" Decode.int
versionsDecoder = Decode.maybe (Decode.field "versions" (Decode.list Decode.int))
versionDecoder = Decode.maybe (Decode.field "version" Decode.int)
created_atDecoder = Decode.field "created_at" (Decode.maybe Extra.iso8601)
creator_Decoder = Decode.field "creator" creatorDecoder
hiddenDecoder = Decode.field "hidden" Decode.bool
unlistedDecoder = Decode.field "unlisted" Decode.bool

previouslyDocDecoder =
    Decode.succeed PreviousArticle
        |> decodeApply idDecoder
        |> decodeApply titleDecoder

previouslyDecoder = Decode.field "previously" (Decode.list previouslyDocDecoder)

-- |> == clojure's ->>
articleDecoder : Decoder Article                    
articleDecoder =
    Decode.succeed Article
        |> decodeApply creator_Decoder
        |> decodeApply tagsDecoder
        |> decodeApply contentDecoder
        |> decodeApply commentsDecoder
        |> decodeApply titleDecoder
        |> decodeApply idDecoder
        |> decodeApply versionsDecoder
        |> decodeApply versionDecoder
        |> decodeApply created_atDecoder
        |> decodeApply hiddenDecoder
        |> decodeApply unlistedDecoder
        |> decodeApply previouslyDecoder
        |> decodeApply (Decode.succeed Murja)

type alias Title =
    { title : String
    , id : Int
    , year : Int
    , month: Int
    , tags: List String
    }
    

sidebarTitleDecoder =
    Decode.succeed Title
        |> decodeApply (Decode.field "Title" Decode.string)
        |> decodeApply (Decode.field "Id" Decode.int)
        |> decodeApply (Decode.field "Year" Decode.int)
        |> decodeApply (Decode.field "Month" Decode.int)
        |> decodeApply (Decode.field "Tags" (Decode.list Decode.string))
