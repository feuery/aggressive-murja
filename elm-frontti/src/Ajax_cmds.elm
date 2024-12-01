module Ajax_cmds exposing (..)

import Article
import User
import Page
import Feeds
import Message exposing (..)
import Http exposing (..)
import Image as Image
import Settings
import Logs
import Json.Decode as Json

getSession =
    Http.get
        { url = "/api/login/session"
        , expect = Http.expectJson GotSession User.userDecoder}

getEditablePosts : Cmd Msg
getEditablePosts =
    Http.get
        { url = "/api/posts/all-titles"
        , expect = Http.expectJson EditableTitlesReceived (Json.list Article.sidebarTitleDecoder) }

getPage page_id =
    Http.get
        { url = "/api/posts/page/" ++ (String.fromInt page_id)
        , expect = Http.expectJson PageReceived Page.pageDecoder}

getPost : Int -> Cmd Msg
getPost post_id =
    Http.get
        { url = "/api/posts/post/" ++ (String.fromInt post_id)
        , expect = Http.expectJson PostReceived Article.articleDecoder}

getSettings =
    Http.get
        { url = "/api/settings/client-settings"
        , expect = Http.expectJson SettingsReceived Settings.settingsDecoder}

getSettingsAdmin =
    Http.get
        { url = "/api/settings/client-settings"
        , expect = Http.expectJson AdminSettingsReceived Settings.settingsDecoder}        

getTitles =
    Http.get
        { url = "/api/posts/titles"
        , expect = Http.expectJson TitlesReceived (Json.list Article.sidebarTitleDecoder)}

postLogin username password =
    Http.post
       { url = "/api/login/login"
       , expect = Http.expectJson LoginSuccess User.userDecoder
       , body = Http.jsonBody <| User.encodeLoggingIn <| User.UserLoggingIn username password}

getPostEditorData post_id =
    Http.get
        { url = "/api/posts/post/" ++ (String.fromInt post_id) ++ "/allow-hidden/true"
        , expect = Http.expectJson EditorPostReceived Article.articleDecoder}

putArticle : Article.Article -> Cmd Msg        
putArticle article =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/api/posts/post"
        , body = Http.jsonBody <| Article.encode article
        , expect = Http.expectString HttpGoHome
        , timeout = Nothing
        , tracker = Nothing
        }
                   
-- returns { :id :name }
getListOfImages : Bool -> Cmd Msg
getListOfImages managerCalled = Http.get
                  { url = "/api/pictures/list/all"
                  , expect = Http.expectJson (GotListOfImages managerCalled) (Json.list Image.imageDecoder)}


postPicture pictureFile = Http.post 
                          { url = "/api/pictures"
                          , body = Http.multipartBody [ Http.filePart "file" pictureFile ]
                          , expect = Http.expectJson UploadedImage Image.imageResponseDecoder }


deletePictures ids = Http.request
                     { url = "/api/pictures"
                     , method = "DELETE"
                     , headers = []
                     , expect = Http.expectString HttpManagerGetListOfImages
                     , body = Http.jsonBody <| (Image.list_of_uuids_encode ids)
                     , timeout = Nothing
                     , tracker = Nothing}

getReferencingPosts id = Http.get
                         { url = "/api/pictures/referencing/" ++ id
                         , expect = Http.expectJson GotReferencingPosts (Json.list Image.referencingPostDecoder)}

loadTaggedPosts tags = Http.get
                       { url = "/api/posts/tagged/" ++ tags
                       , expect = Http.expectJson GotTaggedPosts (Json.list Article.articleDecoder)}

loadPostVersion post_id_int version_id_int =
    let post_id = String.fromInt post_id_int
        version_id = String.fromInt version_id_int in
    Http.get
        { url = "/api/posts/post/" ++ post_id ++ "/version/" ++ version_id
        , expect = Http.expectJson GotOldPost Article.articleDecoder}

generateNewPost =
    Http.request
        { method = "POST"
        , headers = []
        , url = "/api/posts/new_post"
        , body = emptyBody
        , expect = Http.expectJson NewPostGenerated Json.int
        , timeout = Nothing
        , tracker = Nothing
        }
        
saveSettings settings =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/api/settings/client-settings"
        , body = Http.jsonBody (Settings.encodeSettings settings)
        , expect = Http.expectWhatever SettingsSaved
        , timeout = Nothing
        , tracker = Nothing}

searchPreviouslyPosts search_term =
    Http.post
        { url = "/api/posts/search-previously"
        , body = Http.stringBody "application/json" search_term
        , expect = Http.expectJson PreviouslySearchResult (Json.list Article.previouslyDocDecoder)}


loadPreviousArticle post_id =
    Http.get
        { url = "/api/posts/post/" ++ (String.fromInt post_id)
        , expect = Http.expectJson PreviousPostReceived Article.articleDecoder}

getFeeds archived =
    Http.get
        { url = if archived then
                    "/api/user/feeds?archived=archived"
              else
                  "/api/user/feeds"
        , expect = Http.expectJson FeedsReceived (Json.list Feeds.feedDecoder)}

addFeed newFeed =
    Http.post
        { url = "/api/user/feeds"
        , body = Http.jsonBody (Feeds.newFeedEncoder newFeed)
        , expect = Http.expectWhatever FeedAdded }

markFeedItemRead feed_id item_id =
    Http.post
        { url = "/api/user/feeds/" ++ feed_id ++ "/" ++ item_id ++ "/mark-read"
        , body = Http.emptyBody
        , expect = Http.expectWhatever FeedItemReadResponse}

deleteFeed feed_id =
    Http.request
        { url = "/api/user/feeds/" ++ feed_id
        , method = "DELETE"
        , headers = []
        , expect = Http.expectWhatever FeedDeleted
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing}

getFeedMeta =
    Http.get
        { url = "/api/user/feeds/meta"
        , expect = Http.expectJson FeedMetaReceived Feeds.metadataDecoder}

postExcerpt excerpt feed_id =
    Http.post
        { url = " /api/posts/excerpt/" ++ feed_id
        , body = Http.stringBody "text/plain" excerpt
        , expect = Http.expectJson NewPostGenerated Json.int}

getAdminLogs =
    Http.get
        { url = "/api/logs"
        , expect = Http.expectJson GotAdminLogs (Json.list Logs.decoder)}
