module RouteParser exposing (..)

import Url
import Url.Parser exposing (..)
import String exposing (fromInt)
-- http://localhost:3000/blog/post/edit/21
type Route
    = Page Int
    | Post Int
    | PostAdmin
    | MediaManager
    | PostEditor Int
    | TaggedPosts String
    | PostVersion Int Int
    | SettingsEditor
    | FeedReader
    | Home
    | Logs 
    | NotFound
    | OwnUserSettings
    | InitialSetup 

routeParser =
    oneOf
        [ map Page (s "blog" </> (s "page" </> int))
        , map Home Url.Parser.top
        , map Home (s "blog")
        , map PostVersion (s "blog" </> (s "post" </> (int </> (s "version" </> int))))
        , map Post (s "blog" </> (s "post" </> int))
        , map PostEditor (s "blog" </> (s "post" </> (s "edit" </> int)))
        , map MediaManager (s "blog" </> (s "mediamanager"))
        , map SettingsEditor (s "blog" </> (s "settings"))
        , map TaggedPosts (s "blog" </> (s "tags" </> string))
        , map Logs (s "blog" </> (s "logs"))
        , map PostAdmin (s "blog" </> (s "postadmin"))
        , map FeedReader (s "blog" </> (s "feeds"))
        , map OwnUserSettings (s "blog" </> (s "usersettings"))
        , map InitialSetup (s "blog" </> (s "initial-setup"))]
        

url_to_route url =
            Maybe.withDefault NotFound (parse routeParser url)
