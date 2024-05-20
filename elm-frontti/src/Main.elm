port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Http

import Article
import Article_view exposing (articleView)
import Ajax_cmds exposing (..)
import Creator as C
import Page as P
import Settings
import Message exposing (..)
import User
import Topbar
import PostsAdmin
import PostEditor
import SettingsEditor
import Medialist exposing (medialist)
import Image
import ImageSelector exposing (imageSelector)

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
                  , aceStateUpdate AceStateUpdate]

initialModel url key viewstate = Model viewstate Nothing False False [] Nothing LoggedOut key url Nothing Time.utc [] []
    
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
    
init _ url key =
    let (viewstate, cmds) = (viewStatePerUrl url)
        model = initialModel url key viewstate
    in
        ( model
        , Cmd.batch (List.append cmds [ Task.perform AdjustTimeZone Time.here]))


-- UPDATE


-- PORTS --
port prompt : String -> Cmd msg
port alert : String -> Cmd msg
port showPreviousPostsModal: (() -> Cmd msg)
port closePreviousPostsModal: (() -> Cmd msg)
port showPreviousPostPreviewModal: (() -> Cmd msg)
port tags : (String -> msg) -> Sub msg
port aceStateUpdate : (String -> msg) -> Sub msg

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
                    if model.view_state == PostEditor then
                        ({ model | loginState = LoggedIn user 
                         , postEditorSettings = Nothing}
                        , Cmd.none)
                    else 
                        ({model | loginState = LoggedIn user}, Cmd.none)
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
        GotFiles file files ->
            if String.startsWith "image" (mime file) then
                ( { model | draggingImages = False }
                , postPicture file)
            else
                ( { model | draggingImages = False }
                , alert ("Got " ++ (mime file) ++ ", expected an image"))
        GotInputFiles files ->
            if List.all (\file -> String.startsWith "image" (mime file)) files then
                ( model
                , Cmd.batch (List.map (\file -> postPicture file) files))
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
                  , Topbar.topbar model.loginState
                  , div [class "flex-container"] 
                        [ div [class "page"]
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
                                                                   [ div [class "post"] [ text "There are no posts in this instance"]])
                                                            , [footer [(attribute "data-testid" "page-changer")] (if page.id > 1 then [ a [href ("/blog/page/" ++ fromInt (page.id + 1))] [text "Older posts"]
                                                                                                 , a [href ("/blog/page/" ++ fromInt (page.id - 1)), class "newer-post"] [text "Newer posts"]]
                                                                             else [a [href ("/blog/page/" ++ fromInt (page.id + 1))] [text "Next page"]])]])
                                           ShowError err ->
                                               [pre [] [text err]]
                                           PostEditorList titles -> [ PostsAdmin.view titles ]
                                           TaggedPostsView articles ->
                                               (List.map (articleView settings model.loginState model.zone) articles)
                                           PostEditor ->
                                               case model.postEditorSettings of
                                                   Just editorSettings ->
                                                       let post = editorSettings.article
                                                           tag_index = editorSettings.selected_tag in
                                                       PostEditor.postEditor post tag_index model.showImageModal model.loadedImages model.draggingImages editorSettings settings model.zone model.loginState model.searchedPosts
                                                   Nothing -> [ div [] [ text "No post loaded" ]]
                                           MediaList -> [ medialist model.loadedImages model.medialist_state ]
                                           SettingsEditor -> [ SettingsEditor.editor settings])
                        , div [id "sidebar"] [ User.loginView model.loginState
                                             , (sidebarHistory model.titles )
                                             , (case model.view_state of
                                                    PostEditorList titles -> PostsAdmin.tagList titles
                                                    
                                                    _ -> div [] [])]]]}
