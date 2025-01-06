port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Http
import Regex

import Article
import Article_view exposing (articleView)
import Ajax_cmds exposing (..)
import Creator as C
import Page as P
import Settings
import Message exposing (..)
import User
import PostsAdmin
import PostEditor
import SettingsEditor
import Medialist exposing (medialist)
import Image
import Logviewer
import Logs
import ImageSelector exposing (imageSelector)
import InitialForm

import DateTime exposing (DateTime)
import Json.Decode as Decode
import Json.Encode
import Time
import Task
import Dict.Extra exposing (groupBy)
import Dict exposing (toList, keys, get)
import String exposing (fromInt)
import String.Extra exposing (toSentenceCase)
import Stack exposing (push, top, pop)

import Browser.Navigation as Nav

import RouteParser
import Url
import Date_utils exposing (int_to_month_string)

import UUID
import File exposing (mime)

import FeedView
import Feeds exposing (NewFeed)
import Tab exposing (..)
import UserEditor

-- MAIN

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch 
                  [ tags ReceivedTag
                  , aceStateUpdate AceStateUpdate
                  , excerptCreated ExcerptCreated]

initialModel url key viewstate = Model viewstate Nothing False False [] Nothing LoggedOut key url Nothing Time.utc [] [] Nothing PerFeed Nothing False
    
viewStatePerUrl : Url.Url -> (ViewState, List (Cmd Msg))
viewStatePerUrl url =
    case RouteParser.url_to_route url of
        RouteParser.Page page_id -> (Loading, [ getSettings
                                              , getTitles
                                              , getSession
                                              , getPage page_id
                                              ])
        RouteParser.Post post_id -> (Loading, [ getSettings
                                              , getTitles
                                              , getSession
                                              , getPost post_id])
        RouteParser.Home -> (Loading, [ getSettings
                                      , getTitles
                                      , getSession
                                      , getPage 1
                                      ])
        RouteParser.PostEditor post_id -> (Loading, [ getSettings
                                                    , getTitles
                                                    , getSession
                                                    , getPostEditorData post_id])
        RouteParser.PostAdmin -> (Loading, [ getSettings
                                           , getSession
                                           , getTitles
                                           , getEditablePosts ])
        RouteParser.MediaManager -> (Loading, [ getSettings
                                              , getSession
                                              , getTitles
                                              , getListOfImages True] )
        RouteParser.TaggedPosts tags_ -> (Loading, [ getSession
                                                   , getSettings
                                                   , getTitles
                                                   , loadTaggedPosts tags_])

        RouteParser.PostVersion post_id version_id -> (Loading, [ getSession
                                                                , getSettings
                                                                , getTitles
                                                                , loadPostVersion post_id version_id])
                                                                      
        RouteParser.NotFound -> (ShowError ("Couldn't parse url " ++ (Url.toString url)), [Cmd.none])
        RouteParser.SettingsEditor -> (Loading, [ getSession
                                                , getSettingsAdmin 
                                                , getTitles])
        RouteParser.FeedReader -> (Loading, [ getSession
                                            , getSettings
                                            , getFeeds False 
                                            , getFeedMeta ])
        RouteParser.Logs -> ( (Logs [] [] "")
                            , [ getSettings
                              , getSession
                              , getAdminLogs
                              , getTitles
                              , getLogGroups])
        RouteParser.OwnUserSettings -> ( UserSettings "" "" Nothing
                                       , [ getSettings
                                         , getSession
                                         , getTitles])
        RouteParser.InitialSetup -> ( InitialSetup { username = ""
                                                   , nickname = ""
                                                   , password = "" 
                                                   , domain = ""
                                                   , blog_title = ""
                                                   , rss_title = ""
                                                   , rss_link = ""
                                                   , rss_description = Nothing
                                                   , rss_lang = ""
                                                   , rss_email = ""}
                                    , [ getSettings ])

init _ url key =
    let (viewstate, cmds) = (viewStatePerUrl url)
        model = initialModel url key viewstate
    in
        ( model
        , Cmd.batch (List.append cmds [ Task.perform AdjustTimeZone Time.here]))


-- UPDATE


-- PORTS --
port prompt : String -> Cmd msg
port showPreviousPostsModal: (() -> Cmd msg)
port closePreviousPostsModal: (() -> Cmd msg)
port showPreviousPostPreviewModal: (() -> Cmd msg)
port tags : (String -> msg) -> Sub msg
port aceStateUpdate : (String -> msg) -> Sub msg
port showModal: String -> Cmd msg
port createExcerpt: (String, String) -> Cmd msg
port excerptCreated: ((String, String) -> msg) -> Sub msg 

toggleHidden article =
    { article | hidden = not article.hidden}
toggleUnlisted article =
    { article | unlisted = not article.unlisted}

errToString: Http.Error -> String 
errToString err =
    case err of
        Http.BadUrl str -> "Bad url: " ++ str
        Http.Timeout -> "Timeout trying to contact the server. Are you online?"
        Http.NetworkError -> "Network error. Are you online?"
        Http.BadStatus status -> String.fromInt status
        Http.BadBody body -> "Received unparseable response: " ++ body

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SettingsReceived result ->
            case result of
                Ok new_settings ->
                    ({model | settings = Just new_settings}, Cmd.none)
                        
                Err http_error ->
                    ( model
                    , alert ("Error loading settings " ++ Debug.toString http_error))
        PostReceived result ->
            ( {model | view_state = case result of
                                        Ok post -> PostView post 
                                        Err error -> ShowError ( errToString error)}
            , Cmd.none)
        PageReceived result ->
            ( {model | view_state = case result of
                                        Ok page -> PageView page
                                        Err error -> ShowError ( errToString error)}
            , Cmd.none)
        TitlesReceived result ->
            case result of
                Ok decoded_titles ->
                    ({ model | titles = decoded_titles}
                    ,  Cmd.none)
                Err error ->
                    ( model
                    , alert ("Error loading titles " ++ Debug.toString error))
        UrlChanged url ->
            let (view_state, cmds) = viewStatePerUrl url in 
            ({model | url = url, view_state = view_state}, Cmd.batch cmds)
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    (model, Nav.pushUrl model.key (Url.toString url))
                Browser.External href ->
                    (model, Nav.load href)
        LoginFocus ->
            ({model | loginState = LoggingIn "" ""}, Cmd.none)
        ChangeUsername username ->
            case model.loginState of
                LoggingIn old_username password ->
                    ({model | loginState = LoggingIn username password}, Cmd.none)
                _ -> (model, Cmd.none)
        ChangePassword password ->
            case model.loginState of
                LoggingIn username old_password ->
                    ({model | loginState = LoggingIn username password}, Cmd.none)
                _ -> (model, Cmd.none)
        DoLogIn -> case model.loginState of
                       LoggingIn username password ->
                           (model, postLogin username password)
                       _ -> (model, Cmd.none)
        LoginSuccess result ->
            case result of
                Ok user ->
                    ({model | loginState = LoggedIn user}, Cmd.none)
                Err error ->
                    ({model | loginState = LoginFailed}, Cmd.none)
        GotSession result ->
            case result of
                Ok user ->
                    case model.view_state of
                        PostEditor -> 
                            ({ model | loginState = LoggedIn user 
                             , postEditorSettings = Nothing}
                            , getTopbarAlarms user.permissions)
                        UserSettings oldpwd newpwd _ ->
                            ( { model
                                  | loginState = LoggedIn user
                                  , view_state = UserSettings oldpwd newpwd (Just user)}
                            , getTopbarAlarms user.permissions)                                  
                        _ ->
                            ({model | loginState = LoggedIn user}
                            , getTopbarAlarms user.permissions)
                Err error ->
                    case error of
                        Http.BadStatus status ->
                            if status == 401 then
                                -- no valid session
                                (model, Cmd.none)
                            else
                                ( model
                                , alert ("Error (" ++ String.fromInt status ++ ") when loading session"))
                        Http.BadBody err ->
                            ( model
                            , alert ("Error when loading session: " ++ err))
                        _ -> ( model
                             , alert ("Error when loading session"))
        EditableTitlesReceived result ->
            ( {model | view_state = case result of
                                        Ok titles -> PostEditorList titles 
                                        Err error -> ShowError ( errToString error)}
            , Cmd.none)
        OpenPostEditor post_id ->
            (model, getPostEditorData post_id)
        EditorPostReceived result ->
            case result of
                Ok post ->
                    ({ model | view_state = PostEditor
                     , postEditorSettings = Just (PostEditorSettings post "" False Nothing)}
                    , Cmd.none)
                Err error ->
                    ( model
                    , alert ("Error loading post editor " ++ Debug.toString error))
        PromptTag prompt_message ->
            (model, prompt prompt_message)
        Alert alert_msg ->
            (model, alert alert_msg)
        RunAce content ->
            (model, reallySetupAce content)
        SelectTag tag ->
            case model.postEditorSettings of
                Just settings ->
                    ({ model | postEditorSettings = Just
                           { settings | selected_tag = tag}}
                    , Cmd.none)
                _ -> (model, Cmd.none)
        ReceivedTag tag ->
            case model.postEditorSettings of
                Just settings ->
                    let old_article = settings.article
                        article = { old_article | tags = tag :: settings.article.tags} in
                    ({ model | postEditorSettings = Just
                           { settings | article = article}}
                    , Cmd.none)
                Nothing -> (model, alert "ReceivedTag called even though postEditorSettings is nil")
        DropTag tag ->
            case model.postEditorSettings of
                Just settings ->
                    let old_article = settings.article
                        article = { old_article | tags = List.filter ((/=) settings.selected_tag) old_article.tags} in
                    ({ model | postEditorSettings = Just
                           { settings | article = article}}
                    , Cmd.none)
                Nothing -> (model, alert "DropTag called even though postEditorSettings is nil")
        HttpIgnoreResponse result ->
            (model, Cmd.none)
        SavePost article ->
            doGoHome_
              { model | postEditorSettings = Nothing}
              [ putArticle article ]
                    

        GoHome -> doGoHome model
        HttpGoHome _ -> doGoHome model

        AceStateUpdate content ->
            case model.postEditorSettings of
                Just settings ->
                    let article = settings.article in
                    ({ model | postEditorSettings = Just
                           { settings | article =
                                 { article | content = content}}}
                    , Cmd.none)
                Nothing -> (model, alert "AceStateUpdate called even though postEditorSettings is nil")
                    
        ChangeTitle new_title ->
            case model.postEditorSettings of
                Just settings ->
                    let article = settings.article in
                    ({ model | postEditorSettings = Just
                           { settings | article =
                                 { article | title = new_title}}}
                    , Cmd.none)
                Nothing -> (model, alert "ChangeTitle called even though postEditorSettings is nil")            
        HttpManagerGetListOfImages _ -> (model, getListOfImages True)                                  
        GetListOfImages -> ( { model | showImageModal = True }
                           , getListOfImages False)
        GotListOfImages managerCalled result ->
            case result of

                Ok images ->
                    case managerCalled of
                        True ->
                            ({ model
                                 | loadedImages = images
                                 , view_state = MediaList
                                 , medialist_state = Just (MediaListState [] Dict.empty)}
                            , Cmd.batch (List.map (\image -> getReferencingPosts (UUID.toString image.id)) images))
                        False -> 
                            ({model | showImageModal = True, loadedImages = images}, Cmd.none)
                Err error ->
                    ( { model | view_state = ShowError (errToString error) }
                    , Cmd.none)
        SelectedImage img_id ->
            ( {model | showImageModal = False, loadedImages = [] }
            , addImgToAce (UUID.toString img_id))
        EditorDragEnter ->
            ( {model | draggingImages = True}
            , Cmd.none)
        EditorDragLeave ->
            ( {model | draggingImages = False}
            , Cmd.none)
        GotFiles sendPicture file files ->
            if String.startsWith "image" (mime file) then
                ( { model | draggingImages = False }
                , sendPicture file)
            else
                ( { model | draggingImages = False }
                , alert ("Got " ++ (mime file) ++ ", expected an image"))
        GotInputFiles files ->
            if List.all (\file -> String.startsWith "image" (mime file)) files then
                ( model
                , Cmd.batch (List.map (\file -> postPicture UploadedImage PostEditor.editor_image_api file) files))
            else
                ( model
                , alert ("Expected images, got " ++ (String.join ", " (List.map mime files))))
        UploadedImage imgResponse ->
            case imgResponse of
                Ok actualResponse ->
                    ( model
                    , addImgToAce (UUID.toString actualResponse.id ))
                Err err ->
                    (model, alert ("Error uploading image " ++ Debug.toString err))
        MarkImageForRemoval img_id ->
            case model.medialist_state of
                Just state ->
                    if List.member img_id state.selected_ids_for_removal then
                        ({ model | medialist_state = Just {state | selected_ids_for_removal =
                                                               List.filter ((/=) img_id) state.selected_ids_for_removal}}
                        , Cmd.none)
                    else 
                        ({ model | medialist_state = Just {state | selected_ids_for_removal = img_id :: state.selected_ids_for_removal}}
                        , Cmd.none)
                        
                Nothing ->
                    ( model
                    , alert "Medialist state is uninitialized")
        MarkAllImages ids ->
            case model.medialist_state of
                Just state ->
                    ({ model | medialist_state = Just {state | selected_ids_for_removal = ids}}
                    , Cmd.none)
                Nothing -> ( model
                           , alert "Medialist state is uninitialized")
        RemoveSelectedImages ->
            case model.medialist_state of
                Just state -> 
                    (model, deletePictures state.selected_ids_for_removal)
                Nothing -> (model, Cmd.none)
        GotReferencingPosts response ->
            case response of
                Ok posts ->
                    case model.medialist_state of
                        Just state -> ({ model | medialist_state = Just {state | referencing_posts =
                                                                             Dict.union state.referencing_posts (groupBy .media_id posts)}}
                                      , Cmd.none)
                        Nothing -> ( model
                                   , Cmd.none)
                Err err ->
                    ( model
                    , alert "Error while downloading info about referencing posts, check your devtools' network log")
        PushUrl url ->
            ( model, Nav.pushUrl model.key url )
        AdjustTimeZone zone ->
            ( {model | zone = zone}
            , Cmd.none)
        GotTaggedPosts result ->
            case result of
                Ok posts ->
                    ({ model | view_state = TaggedPostsView posts}
                    , Cmd.none)
                Err err ->
                    ( model , alert ( "Error loading tagged posts " ++ (Debug.toString err)))
        ToggleArticlePreview ->
            ({ model | postEditorSettings = Maybe.map (\settings ->
                                                           {settings | show_preview = not settings.show_preview}) model.postEditorSettings}
            , Cmd.none)
        GotOldPost result ->
            case result of
                Ok post ->
                    ({ model | view_state = PostView post}
                    , Cmd.none)
                Err err ->
                    (model , alert ("Error loading post version " ++ Debug.toString err))
        GenNewPost ->
            ( model
            , generateNewPost)
        NewPostGenerated new_post_id ->
            case new_post_id of
                Ok id -> 
                    ( model
                    , Cmd.batch
                        [ ( Nav.pushUrl model.key ("/blog/post/edit/" ++ String.fromInt id))
                        , getPostEditorData id])
                Err error ->
                    ( model
                    , alert ("ERROR: " ++ (Debug.toString error)))
        ToggleArticleHidden ->
            ({ model | postEditorSettings = Maybe.map (\settings ->
                                                           {settings | article = toggleHidden settings.article})
                   model.postEditorSettings}
            , Cmd.none)
        ToggleArticleUnlisted ->
            ({ model | postEditorSettings = Maybe.map (\settings ->
                                                           {settings | article = toggleUnlisted settings.article})
                   model.postEditorSettings}
            , Cmd.none)
        AdminSettingsReceived result -> 
            case result of
                Ok new_settings ->
                    ({model
                         | settings = Just new_settings
                         , view_state = SettingsEditor}, Cmd.none)
                        
                Err http_error ->
                    ( model
                    , alert ("Error loading settings " ++ Debug.toString http_error))
        SetTimeFormat tf ->
            ({ model | settings = Maybe.map (\settings ->
                                                 { settings | time_format = tf})
                   model.settings}
            , Cmd.none)
        SetBlogTitle title ->
            ({ model | settings = Maybe.map (\settings ->
                                                 { settings | blog_title = title})
                   model.settings}
            , Cmd.none)
        SetPageSize pg_size ->
            case String.toInt pg_size of
                Just page_size -> 
                    ({ model | settings = Maybe.map (\settings ->
                                                         { settings | recent_post_count = page_size})
                           model.settings}
                    , Cmd.none)
                Nothing ->
                    ( model
                    , alert "Page size should be a number")
        SaveSettings ->
            case model.settings of
                Just settings -> 
                    ( model
                    , saveSettings settings)
                Nothing ->
                    ( model
                    , Cmd.none)
        SettingsSaved result ->
            case result of
                Ok _ ->
                    ( model
                    , Cmd.none)
                        
                Err http_error ->
                    ( model
                    , alert ("Error saving settings " ++ Debug.toString http_error))
        ShowPreviousPostsModal ->
            ( model
            , showPreviousPostsModal ())
        ClosePreviousPostsModel ->
            ( model
            , closePreviousPostsModal ())
        PreviouslySearchInput search_term ->
            ( model
            , searchPreviouslyPosts search_term)
        PreviouslySearchResult result ->
            case result of
                Ok previously_posts ->
                    let article_previouslies = case model.postEditorSettings of
                                                    Just settings -> settings.article.previously
                                                    Nothing -> []
                    in
                    ({ model |
                           searchedPosts = List.filter (\p -> not (List.member p article_previouslies)) previously_posts}
                    , Cmd.none)
                Err error ->
                    ( model
                    , alert (errToString error))
        SelectPreviouslyPost selectedPost  ->
            case model.postEditorSettings of
                Just editorSettings -> 
                    let new_posts = List.filter ((/=) selectedPost) model.searchedPosts
                        article = editorSettings.article
                        previously = article.previously
                    in
                        ({ model
                             | searchedPosts = new_posts
                             , postEditorSettings = Just { editorSettings
                                                             | article = { article
                                                                             | previously = selectedPost :: previously}}}
                        , Cmd.none)
                Nothing ->
                    ( model
                    , Cmd.none)
        DropPreviously previous_post ->
            case model.postEditorSettings of
                Just editorSettings -> 
                    let article = editorSettings.article
                        previously = article.previously
                    in
                        ({ model
                             | postEditorSettings = Just
                               { editorSettings
                                     | article =
                                     { article
                                           | previously = List.filter ((/=) previous_post) previously}}}
                        , Cmd.none)
                Nothing ->
                    ( model
                    , Cmd.none)
        SetPreviouslyLabel label ->
            ({ model | settings = Maybe.map (\settings ->
                                                 { settings | previously_label = label})
                   model.settings}
            , Cmd.none)
        LoadPreviouslyPreview prev_article ->
            ( model
            , loadPreviousArticle prev_article.id )
        PreviousPostReceived result ->
            case result of
                Ok article ->
                    let postEditorSettings = model.postEditorSettings
                    in
                        ({ model
                             | postEditorSettings = Maybe.map (\settings ->
                                                                   { settings
                                                                         | previewing_previously = Just article}) postEditorSettings}
                        , showPreviousPostPreviewModal ())
                Err err ->
                    ( model
                    , alert ("Failed to load a previosly-post with error: " ++ (errToString err)))
        ClosePreviousPostPreviewModal ->
            ( model
            , closePreviousPostsModal ())
        FeedsReceived result -> 
            case result of
                Ok fs ->
                    let feeds = (  fs
                                |> List.sortBy (\f -> (  f.items
                                                      |> List.map (Time.posixToMillis << .pubdate)
                                                      |> List.minimum
                                                      |> Maybe.withDefault 999))
                                |> List.reverse) 
                    in
                    case model.view_state of
                        Feeds _ archived -> 
                            ( { model | view_state = Feeds feeds archived}
                            , Cmd.none)
                        _ ->
                            ( { model | view_state = Feeds feeds False}
                            , Cmd.none)
                Err error -> 
                    ( { model | view_state = ShowError (errToString error) }
                    , Cmd.none)
        SetFeedName name ->
            let new_feed = (Maybe.withDefault (NewFeed "" "") model.new_feed)
            in
            ({ model
                 | new_feed = Just { new_feed
                                       | name = name}}
            , Cmd.none)
        SetFeedUrl url ->
            let new_feed = (Maybe.withDefault (NewFeed "" "") model.new_feed)
            in
            ({ model
                 | new_feed = Just { new_feed
                                       | url = url}}
            , Cmd.none)
        AddFeed new_feed ->
            ({ model
                 | new_feed = Nothing}
            , addFeed new_feed)
        FeedAdded r ->
            case r of
                Ok _ ->
                    case model.view_state of
                        Feeds _ archived -> 
                            ( model
                            , getFeeds archived)
                        _ -> ( model
                             , Cmd.none)
                Err error ->
                    ( { model | view_state = ShowError (errToString error) }
                    , Cmd.none)
        SelectTab tab_id selected_tab ->
            case tab_id of 
                "rss-feed-tab" ->
                    case (str_to_readerState selected_tab) of
                        Just readerstate -> 
                            ({ model
                                 | feedReaderState = readerstate}
                            , Cmd.none)
                        Nothing ->
                            ( model
                            , alert <| "Unknown selected tab " ++ selected_tab)
                "posteditor-preview-tab" ->
                    ({ model |
                           postEditorSettings = 
                           Maybe.map (\settings -> {settings
                                                       | show_preview = selected_tab == "PreviewArticle"})
                           model.postEditorSettings}
                    , Cmd.none)
                _ -> ( model
                     , alert <| "Unknown tab " ++ tab_id)
        ReadFeedItem feed_id item_id is_read ->
            case model.view_state of
                Feeds feeds show_archived ->
                    let new_feeds = (  feeds
                                    |> List.map (\f -> if f.id == feed_id then
                                                           {f | items = (  f.items
                                                                        |> List.map (\item ->
                                                                                         if item.id == item_id then
                                                                                             {item | is_read = is_read}
                                                                                         else
                                                                                             item ))}
                                                       else
                                                           f))
                    in
                        ({ model | view_state = Feeds new_feeds show_archived}
                        , markFeedItemRead (UUID.toString feed_id) (UUID.toString item_id))
                _ -> ( model
                     , Cmd.none)
        ShowArchivedFeedItems showArchived ->
            case model.view_state of
                Feeds feeds _ -> 
                    ({ model
                         | view_state = Feeds feeds showArchived}
                    , getFeeds showArchived)
                _ -> ( model
                     , Cmd.none)
        FeedItemReadResponse result ->
            case result of
                Ok _ -> ( model
                        , Cmd.none)
                Err error -> ( { model | view_state = ShowError (errToString error) }
                             , Cmd.none)
        DeleteFeed id ->
            ( model 
            , deleteFeed <| UUID.toString id)
        FeedDeleted result ->
            case result of 
                Ok _ ->
                    case model.view_state of
                        Feeds _ archived ->
                            ( model
                            , getFeeds archived)
                        _ ->
                            ( model
                            , Cmd.none)
                Err error -> ( { model | view_state = ShowError (errToString error) }
                             , Cmd.none)

        FeedMetaReceived result ->
            case result of
                Ok metadata ->
                    ({ model
                         | feedMetadata = Just metadata}
                    , Cmd.none)
                Err error -> ( { model | view_state = ShowError (errToString error) }
                             , Cmd.none)
        SelectExcerpt article_uuid ->
            ( model
            , showModal <| "excerpt-dialog-" ++ (UUID.toString article_uuid))
        CreateExcerptPost textarea_id feed_id ->
            ( model
            , createExcerpt (textarea_id, UUID.toString feed_id))
        ExcerptCreated (excerpt, feed_id) ->
            ( model
            , postExcerpt excerpt feed_id)
        GotAdminLogs result ->
            case result of
                Ok logs ->
                    case model.view_state of
                        Logs _ g str ->
                            ({ model | view_state = Logs logs g str}
                            , Cmd.none)
                        _ ->
                            ( model
                            , alert "Wrong viewstate again")
                Err error ->
                    ( { model | view_state = ShowError (errToString error) }
                    , Cmd.none)
        EditGroupRegexp new_regex ->
            ((case model.view_state of
                 Logs a b _ -> 
                     { model | view_state = Logs a b new_regex}
                 _ -> model)
            , Cmd.none)
        SaveLogGroup new_potential_regex ->
            case Regex.fromString new_potential_regex of
                Just _ ->
                    case model.view_state of
                        Logs logs groups asd ->
                            let group = Logs.str_to_group new_potential_regex
                                new_groups = (group::groups) in 
                            ({ model | view_state = Logs logs new_groups asd }
                            , saveGroups new_groups)
                        _ -> ( model
                             , Cmd.none)
                Nothing ->
                    ( model
                    , alert <| "Invalid regex " ++ new_potential_regex)
        DeleteLogGroup group ->
            case model.view_state of
                Logs logs groups current ->
                    let new_groups = (List.filter (\g -> g.name /= group) groups)
                        new_state = Logs logs new_groups current
                    in
                        ({ model | view_state = new_state}
                        , saveGroups new_groups)
                _ -> ( model
                     , Cmd.none)
        SetLogAlarmy group new_alarmy ->
            case model.view_state of
                Logs a groups b ->
                    let new_groups = groups
                                     |> List.map (\g ->
                                                      if g.name == group.name then
                                                          { g | alarmy = new_alarmy}
                                                      else g)
                    in
                        ({ model
                               | view_state = Logs a new_groups b}
                        , saveGroups new_groups)
                _ -> ( model
                     , Cmd.none)
        LogGroupsSaved result ->
            case result of
                Ok _ -> 
                    ( model
                    , Cmd.none)
                Err err ->
                    ( model
                    , alert <| "Saving groups failed " ++ (Debug.toString err))
        GotLogGroups result ->
            case result of
                Ok groups ->
                    let new_viewstate = 
                            case model.view_state of
                                Logs logs _ str -> Logs logs groups str
                                _ -> model.view_state
                    in
                        ({ model
                             | view_state = new_viewstate}
                        , case model.view_state of
                              Logs _ _ _ -> Cmd.none 
                              _ -> alert "view state is wrong")
                Err error -> 
                    ( { model | view_state = ShowError (errToString error) }
                    , Cmd.none)
        GotTopbarLogAlarm res ->
            case res of
                Ok alarm ->
                    ({ model | ringLogAlarm = alarm.alarm}
                    , Cmd.none)
                Err error -> 
                    ( { model | view_state = ShowError (errToString error) }
                    , Cmd.none)
        SetDomain dm ->
            ({ model | settings = Maybe.map (\settings ->
                                                 { settings | domain = dm })
                   model.settings}
            , Cmd.none)
        SetUsername usrname ->
            case model.view_state of
                UserSettings oldpwd newpwd usr ->
                    ({ model
                         | view_state = UserSettings oldpwd newpwd (Maybe.map (\old_usr ->
                                                                                   {old_usr | username = usrname}) usr)}
                    , Cmd.none)
                _ -> ( model, Cmd.none)
        SetNickname new_nickname ->
            case model.view_state of
                UserSettings oldpwd newpwd usr ->
                    ({ model
                         | view_state = UserSettings oldpwd newpwd (Maybe.map (\old_usr ->
                                                                                   {old_usr | nickname = new_nickname}) usr)}
                    , Cmd.none)
                _ -> ( model, Cmd.none)
        SetNewpwd newpwd ->
            case model.view_state of
                UserSettings oldpwd _ usr ->
                    ({ model
                         | view_state = UserSettings oldpwd newpwd usr}
                    , Cmd.none)
                _ -> ( model, Cmd.none)
        SetOldpwd oldpwd ->
            case model.view_state of
                UserSettings _ newpwd usr ->
                    ({ model
                         | view_state = UserSettings oldpwd newpwd usr}
                    , Cmd.none)
                _ -> ( model, Cmd.none)
        SubmitChangedUser oldpasswd newpasswd user ->
            -- TODO implement 
            case model.loginState of
                LoggedIn usr ->
                    ( model
                    , if usr.id == user.id then
                          submitUser user oldpasswd newpasswd  
                      else
                          Cmd.none)
                _ -> ( model
                     , Cmd.none)
        UserSubmitResult r ->
            case r of
                Ok _ ->
                    ( model
                    , Cmd.batch [ getSettings
                                , getSession
                                , getTitles])
                Err error ->
                    ( { model | view_state = ShowError (errToString error) }
                    , Cmd.none)
        UploadedOwnProfilePic r ->
            case r of
                -- we're not really interested in the return value more than "is it 2xx instead of 4xx or 5xx?", we're just as api-compatible with the posteditor's image upload functionality as possible
                Ok _ ->
                    ( model
                    , Cmd.batch [ getSettings
                                , getSession
                                , getTitles])
                Err error ->
                    ( { model | view_state = ShowError (errToString error) }
                    , Cmd.none)
        SetInitialUsername usrname ->
            case model.view_state of
                InitialSetup formdata ->
                    ( { model | view_state
                            = InitialSetup { formdata
                                                 | username = usrname }}
                    , Cmd.none)
                _ -> ( model, Cmd.none)
        SetInitialNickname nckname ->
            case model.view_state of
                InitialSetup formdata ->
                    ( { model | view_state
                            = InitialSetup { formdata
                                                 | nickname = nckname }}
                    , Cmd.none)
                _ -> ( model, Cmd.none)
        SetInitialPassword passwd ->
            case model.view_state of
                InitialSetup formdata ->
                    ( { model | view_state
                            = InitialSetup { formdata
                                                 | password = passwd }}
                    , Cmd.none)
                _ -> ( model, Cmd.none)
        SetInitialDomain dmain ->
            case model.view_state of
                InitialSetup formdata ->
                    ( { model | view_state
                            = InitialSetup { formdata
                                                 | domain = dmain }}
                    , Cmd.none)
                _ -> ( model, Cmd.none)
        SetInitialBlog_Title title ->
            case model.view_state of
                InitialSetup formdata ->
                    ( { model | view_state
                            = InitialSetup { formdata
                                                 | blog_title = title }}
                    , Cmd.none)
                _ -> ( model, Cmd.none)
        SetInitialRss_Title title ->
            case model.view_state of
                InitialSetup formdata ->
                    ( { model | view_state
                            = InitialSetup { formdata
                                                 | rss_title = title }}
                    , Cmd.none)
                _ -> ( model, Cmd.none)
        SetInitialRss_Link link ->
            case model.view_state of
                InitialSetup formdata ->
                    ( { model | view_state
                            = InitialSetup { formdata
                                                 | rss_link = link }}
                    , Cmd.none)
                _ -> ( model, Cmd.none)
        SetInitialRss_Description descr ->
            case model.view_state of
                InitialSetup formdata ->
                    ( { model | view_state
                            = InitialSetup { formdata
                                                 | rss_description = (if descr == "" then
                                                                          Nothing
                                                                      else
                                                                          Just descr)}}
                    , Cmd.none)
                _ -> ( model, Cmd.none)
        SetInitialRss_Lang lang ->
            case model.view_state of
                InitialSetup formdata ->
                    ( { model | view_state
                            = InitialSetup { formdata
                                                 | rss_lang = lang }}
                    , Cmd.none)
                _ -> ( model, Cmd.none)
        SetInitialRss_Email email ->
            case model.view_state of
                InitialSetup formdata ->
                    ( { model | view_state
                            = InitialSetup { formdata
                                                 | rss_email = email }}
                    , Cmd.none)
                _ -> ( model, Cmd.none)
        SaveInitialData data ->
            ( model
            , postInitialData data )
        PostInitialSuccess res ->
            case res of
                Ok _ ->
                    doGoHome model
                Err error ->
                    ( { model | view_state = ShowError (errToString error) }
                    , Cmd.none)                    
                    
doGoHome_ model other_cmds =
    (model, Cmd.batch (List.append [ getSettings
                                   , getTitles
                                   , getSession
                                   , getPage 1
                                   , Nav.pushUrl model.key "/blog/"]
                           other_cmds))

doGoHome model = doGoHome_ model []

-- VIEW

sidebarHistory : List Article.Title -> Html Msg
sidebarHistory titles =
    let grouped_by_year = groupBy .year titles in
      div [id "grouper"]
          [ul []
               (List.concat (List.map (\year ->
                                           case get year grouped_by_year of
                                               Just per_year ->
                                                   [li [] [details [] [summary [] [text ((fromInt year) ++ " (" ++ (fromInt (List.length per_year)) ++ ")")],
                                                                        let grouped_by_month = groupBy .month per_year in
                                                                          ul [] (List.concat (List.map (\month ->
                                                                                                            case (int_to_month_string month) of
                                                                                                                Just month_str ->
                                                                                                                    let month_titles = titles |> List.filter (\title ->
                                                                                                                                                                  title.year == year && title.month == month)
                                                                                                                    in
                                                                                                                        [li [] [details [] [summary [] [text ((toSentenceCase month_str) ++ " (" ++ (fromInt (List.length month_titles)) ++ ")")]
                                                                                                                               , ul [class "title-list"] (month_titles
                                                                                                                                                         |> List.map (\title ->
                                                                                                                                                                          [li [class "title-list"]
                                                                                                                                                                               [a [href ("/blog/post/" ++ (fromInt title.id))] [text title.title]]])
                                                                                                                                                         |> List.concat)]]]
                                                                                                                Nothing -> [li [] [details [] [summary [] [text ("Couldn't decode month " ++ (String.fromInt month))]]]]
                                                                                                       ) (keys grouped_by_month) |> List.reverse))]]]

                                               Nothing ->
                                                        [li [] [text ("There's no year " ++ (fromInt year) ++ " in titles")]]) (keys grouped_by_year |> List.reverse)))]


page_wrapper comp = 
    div [class "flex-container"] 
        [ div [class "page"]
              comp]

unknown_state = [ div [] [ text "Unknown viewstate in blog_tab"] ]            

blog_tab settings model =
    div [] 
    (case model.view_state of
        Loading ->
            [div [] [text "LOADING"]]
        PostView article ->
            [ articleView settings model.loginState model.zone article ]
        PageView page ->
            let post_elements = (List.map (articleView settings model.loginState model.zone) page.posts) in
            (List.concat [ (if post_elements /= [] then
                                post_elements
                            else
                                [ div [class "post"] [ text <| case model.loginState of
                                                                   LoggedIn usr -> "Hi " ++ usr.nickname ++ ", there are no (also: no user) posts in this instance"
                                                                   _ -> "There are no (also: no user) posts in this instance"]])
                         , [footer [ attribute "data-testid" "page-changer"
                                   , class "page-changer" ]
                                (if page.id > 1 then
                                     [ a [href ("/blog/page/" ++ fromInt (page.id + 1))] [text "Older posts"]
                                     , a [href ("/blog/page/" ++ fromInt (page.id - 1)), class "newer-post"] [text "Newer posts"]]
                                 else
                                     [a [href ("/blog/page/" ++ fromInt (page.id + 1))] [text "Older posts"]])]])
        ShowError err ->
            [pre [] [text err]]
        TaggedPostsView articles ->
            (List.map (articleView settings model.loginState model.zone) articles)
        Logs logs groups current_group -> [ Logviewer.tab logs groups current_group ]
        -- ignored cases (that should maybe be removed from the enumeration?) that are inlined here to make compiler yell about new unimplemented enumerations
        PostEditorList _ -> unknown_state
        PostEditor -> unknown_state
        MediaList -> unknown_state
        SettingsEditor -> unknown_state
        Feeds _ _ -> unknown_state
        UserSettings oldpasswd newpasswd usr_ -> case usr_ of
                                 Just usr -> [ UserEditor.editor model.draggingImages oldpasswd newpasswd usr]
                                 Nothing -> [ div [] [ text "Can't change user settings when there's no user"]]
        InitialSetup data -> [ InitialForm.initialForm data]
    )

rss_tab model settings =
    div []
    (case model.view_state of
        Feeds feeds show_archived -> [ FeedView.feeds model.feedReaderState model.loginState show_archived settings model.zone feeds model.new_feed model.feedMetadata]
        _ -> [ div [] [ text "Unknown viewstate in rss_tab"] ])

postmanager_tab model =
    div [] 
    (case model.view_state of
        PostEditorList titles -> [ PostsAdmin.view titles ]
        _ -> [ div [] [ text "Unknown viewstate in postmanager_tab"] ])

mediamanager_tab model =
    div [] 
    (case model.view_state of
        MediaList -> [ medialist model.loadedImages model.medialist_state ]
        _ -> [ div [] [ text "Unknown viewstate in mediamanager_tab"] ])

settings_tab settings model =
    div []
        (case model.view_state of
            SettingsEditor -> [ SettingsEditor.editor settings]
            _ -> [ div [] [ text "Unknown viewstate in settings_tab"] ])            
            
posteditor_tab settings model =
    div [ class "posteditor-tab" ]
    (case model.view_state of 
        PostEditor ->
            case model.postEditorSettings of
                Just editorSettings ->
                    let post = editorSettings.article
                        tag_index = editorSettings.selected_tag in
                    PostEditor.postEditor post tag_index model.showImageModal model.loadedImages model.draggingImages editorSettings settings model.zone model.loginState model.searchedPosts
                Nothing -> [ div [] [ text "No post loaded" ]]
        _ -> [ div [] [ text "Unknown viewstate in posteditor_tab" ]])
                                                              
view : Model -> Browser.Document Msg
view model =
    case model.settings of
        Nothing ->
            { title = "Error loading murja"
            , body = 
                  [div [] [text "Couldn't load settings"]]}
        Just settings ->
            { title = settings.blog_title
            , body = 
                  [ header [] [a [href "/"] [text settings.blog_title ]]
                  , div [ class "sidebar-flex" ]
                      [ let tabstate = viewstate_to_tabstate model.view_state in 
                        tabs "topbar" (tabstate_to_str tabstate) (case model.loginState of
                                                                      LoggedIn usr -> Just usr
                                                                      _ -> Nothing)
                            (Dict.fromList [ ("Blog"
                                             , TabEntry "Home" ""
                                                 (blog_tab settings model)
                                                 (Just GoHome)
                                                 ["*"])
                                           , ("RssFeeds"
                                             , TabEntry "RSS Feeds" ""
                                                 (rss_tab model settings)
                                                 (Just (PushUrl "/blog/feeds"))
                                                 ["create-post"] ) -- <- TODO make a real permission for rss
                                           , ("ManagePosts"
                                             , TabEntry "Manage posts" ""
                                                 (postmanager_tab model)
                                                 (Just (PushUrl "/blog/postadmin"))
                                                 ["create-post", "delete-post", "edit-post"])
                                           , ("ManageMedia"
                                             , TabEntry "Manage media" ""
                                                 (mediamanager_tab model)
                                                 (Just (PushUrl "/blog/mediamanager"))
                                                 ["create-post", "delete-post", "edit-post"])
                                           , ("SettingsTab"
                                             , TabEntry "Settings" ""
                                                 (settings_tab settings model)
                                                 (Just (PushUrl "/blog/settings"))
                                                 ["update-settings"])
                                           , ("SettingLogs"
                                             , TabEntry ("Logs" ++ (if model.ringLogAlarm then " (!!!!!!)" else ""))
                                                 (if model.ringLogAlarm then " alert " else "")
                                                 (text "in the frontpage these views don't show up anywhere")
                                                 (Just (PushUrl "/blog/logs"))
                                                 ["update-settings"])
                                           , ("PostEditTab"
                                             , TabEntry "Post editor" ""
                                                 (posteditor_tab settings model)
                                                 (Just GenNewPost)
                                             ["create-post", "edit-post"])])
                      , div [id "sidebar"] [ UserEditor.loginView model.loginState
                                           , (sidebarHistory model.titles )
                                           , (case model.view_state of
                                                  PostEditorList titles -> PostsAdmin.tagList titles
                                                  _ -> div [] [])]]]}
