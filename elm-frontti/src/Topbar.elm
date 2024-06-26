module Topbar exposing (..)

import Message exposing (..)
import User
import Article exposing (..)
import Creator exposing (..)
import Ajax_cmds exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser.Navigation as Nav

import Button exposing (murja_button)

topbar_list =
    [ li [] [ murja_button [ onClick GoHome, attribute "data-testid" "home"]
                  [text "Home"]]
    , li [] [ murja_button [ onClick (PushUrl "/blog/feeds") ] [ text "RSS Feeds" ]]
    , li [] [ murja_button [ onClick (PushUrl "/blog/postadmin"), attribute "data-testid" "manage-posts-btn" ]
                  [text "Manage posts"]]
    , li [] [ murja_button [ onClick (PushUrl "/blog/mediamanager")]
                  [text "Manage media"]]
    , li [] [ murja_button [ onClick (PushUrl "/blog/settings")]
                  [ text "Settings" ]]
    , li [] [ murja_button [ onClick GenNewPost
                           , attribute "data-testid" "new-post-btn" ]
                  [text "New post!"]]]
    
topbar state =
    case state of
        LoggedIn user ->                               
            div [class "left-sidebar"] [ span [] [text ("Welcome, " ++ user.nickname)]
                                       , User.user_avatar user
                                       , ul [] topbar_list ]
        _ -> div [] []
