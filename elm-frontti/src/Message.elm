port module Message exposing (..)

import Http
import Html
import Html.Attributes
import Json.Encode
import Browser
import Time
import Page as P
import Article
import Browser.Navigation as Nav
import Settings
import Url
import Title
import Feeds
import Image exposing (Image, ReferencingPost)
import Logs 

import File exposing (File)
import UUID exposing (UUID)
import Stack exposing (..)
import Dict exposing (Dict)
import Regex 
    
type ViewState
    = PageView P.Page
    | PostView Article.Article
    | Loading 
    | ShowError String
    | PostEditorList (List Title.Title)                     -- list all the posts in db
    | PostEditor
    | MediaList                     -- list all the image blobs in db
    | TaggedPostsView (List Article.Article)
    | SettingsEditor
    | Feeds (List Feeds.Feed) Bool -- <- show_archived?
                                   -- v- the second List will be parsed as List Regex later
    | Logs (List Logs.Log) (List Logs.Group) String

-- a simplified version of ViewState type for the main.elm's tabcomponent 
type TabState
    = Blog
    | RssFeeds
    | ManagePosts
    | ManageMedia
    | SettingsTab 
    | PostEditTab

viewstate_to_tabstate vs =
    case vs of
        PageView _ -> Blog
        PostView _ -> Blog
        Loading -> Blog
        ShowError _ -> Blog
        PostEditorList _ -> ManagePosts
        PostEditor -> PostEditTab
        MediaList -> ManageMedia
        TaggedPostsView _ -> Blog
        SettingsEditor -> SettingsTab
        Feeds _ _ -> RssFeeds
        Logs _ _ _ -> Blog

tabstate_to_str tb =
    case tb of
        Blog -> "Blog"
        RssFeeds -> "RssFeeds"
        ManagePosts -> "ManagePosts"
        ManageMedia -> "ManageMedia"
        SettingsTab  -> "SettingsTab"
        PostEditTab -> "PostEditTab"
                       
str_to_tabstate str =
    case str of
        "Blog" -> Blog
        "RssFeeds" -> RssFeeds
        "ManagePosts" -> ManagePosts
        "ManageMedia" -> ManageMedia
        "SettingsTab"  -> SettingsTab
        "PostEditTab" -> PostEditTab
        _ -> Blog

type alias User =
    { username : String
    , nickname : String
    , img_location : String
    }

type LoginState
    = LoggedIn LoginUser
    | LoggingIn String String
    | LoginFailed
    | LoggedOut      


type alias LoginUser =
    { nickname : String
    , username : String
    , img_location : String
    , primary_group_name : String
    , permissions : List String
    }

type alias MediaListState =
    { selected_ids_for_removal : List UUID
    , referencing_posts : Dict String (List ReferencingPost)}

type alias PostEditorSettings =
    { article : Article.Article
    , selected_tag : String
    , show_preview : Bool
    , previewing_previously : Maybe Article.Article}

type FeedReaderState
    = PerFeed
    | SingleFeed
    | FeedManager 
      
str_to_readerState str =
    case str of
        "PerFeed" -> Just PerFeed
        "SingleFeed" -> Just SingleFeed
        "FeedManager" -> Just FeedManager
        _ -> Nothing
             
type alias Model =
    { view_state : ViewState
    , settings : Maybe Settings.Settings
    , showImageModal : Bool
    , draggingImages : Bool
    , loadedImages : List Image
    , medialist_state : Maybe MediaListState
    , loginState : LoginState
    , key : Nav.Key
    , url : Url.Url
    , postEditorSettings: Maybe PostEditorSettings
    , zone : Time.Zone
    , titles : List Article.Title
    , searchedPosts : List Article.PreviousArticle
    , new_feed: Maybe Feeds.NewFeed
    , feedReaderState: FeedReaderState
    , feedMetadata: Maybe Feeds.FeedMetadata}
    
type Msg
  = PageReceived (Result Http.Error P.Page)
  | PostReceived (Result Http.Error Article.Article)
  | SettingsReceived (Result Http.Error Settings.Settings)
  | TitlesReceived (Result Http.Error (List Article.Title))
  | EditableTitlesReceived (Result Http.Error (List Article.Title))
  | UrlChanged Url.Url
  | LinkClicked Browser.UrlRequest
  | LoginFocus
  | ChangeUsername String
  | ChangePassword String
  | DoLogIn
  | LoginSuccess (Result Http.Error LoginUser)
  | GotSession (Result Http.Error LoginUser)
  | OpenPostEditor Int
  | EditorPostReceived (Result Http.Error Article.Article)
  | PromptTag String
  | ReceivedTag String
  | AceStateUpdate String
  | SelectTag String
  | Alert String
  | DropTag String
  | SavePost Article.Article
  | HttpIgnoreResponse (Result Http.Error String)
  | HttpGoHome (Result Http.Error String)
  | GoHome
  | ChangeTitle String
  | RunAce String
  | GetListOfImages
  | GotListOfImages Bool (Result Http.Error (List Image.Image))
  | SelectedImage UUID
  | EditorDragEnter
  | EditorDragLeave
  | GotFiles File (List File)
  | GotInputFiles (List File)
  | UploadedImage (Result Http.Error Image.PostImageResponse)
  | MarkImageForRemoval UUID
  | MarkAllImages (List UUID)
  | RemoveSelectedImages
  | HttpManagerGetListOfImages (Result Http.Error String)
  | GotReferencingPosts (Result Http.Error (List Image.ReferencingPost))
  | PushUrl String
  | AdjustTimeZone Time.Zone
  | GotTaggedPosts  (Result Http.Error (List Article.Article))
  | ToggleArticlePreview
  | GotOldPost (Result Http.Error Article.Article)
  | GenNewPost 
  | NewPostGenerated (Result Http.Error Int)
  | ToggleArticleUnlisted
  | ToggleArticleHidden
  | AdminSettingsReceived (Result Http.Error Settings.Settings)
  | SetTimeFormat String
  | SetBlogTitle String
  | SetPageSize String
  | SaveSettings
  | SettingsSaved (Result Http.Error ())
  | ShowPreviousPostsModal
  | ClosePreviousPostsModel
  | PreviouslySearchInput String
  | PreviouslySearchResult (Result Http.Error (List Article.PreviousArticle))
  | SelectPreviouslyPost Article.PreviousArticle
  | DropPreviously Article.PreviousArticle
  | SetPreviouslyLabel String
  | LoadPreviouslyPreview Article.PreviousArticle
  | PreviousPostReceived (Result Http.Error Article.Article)
  | ClosePreviousPostPreviewModal
  | FeedsReceived (Result Http.Error (List Feeds.Feed))
  | SetFeedUrl String 
  | SetFeedName String
  | AddFeed Feeds.NewFeed
  | FeedAdded (Result Http.Error ())
  | SelectTab String String
  | ReadFeedItem UUID UUID Bool
  | ShowArchivedFeedItems Bool
  | FeedItemReadResponse (Result Http.Error ())
  | DeleteFeed UUID
  | FeedDeleted (Result Http.Error ())
  | FeedMetaReceived (Result Http.Error Feeds.FeedMetadata)
  | SelectExcerpt UUID
  | CreateExcerptPost String UUID 
  | ExcerptCreated (String, String)
  | GotAdminLogs (Result Http.Error (List Logs.Log))
  | EditGroupRegexp String
  | SaveLogGroup String
  | DeleteLogGroup String
  | SetLogAlarmy Logs.ParsedGroup Bool
  | LogGroupsSaved (Result Http.Error ())
  | GotLogGroups (Result Http.Error (List Logs.Group))

-- ports
port reallySetupAce : String -> Cmd msg
port addImgToAce : String -> Cmd msg

-- dumb shit that would deserve its own module
dangerouslySetInnerHTML: String -> Html.Attribute msg
dangerouslySetInnerHTML = Json.Encode.string >> Html.Attributes.property "dangerouslySetInnerHTML"
