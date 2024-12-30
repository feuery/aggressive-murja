module UserEditor exposing (..)

import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (..)

import Message exposing (..)
import PostEditor exposing (hijackOn)

import Json.Decode as D

-- dropDecoder : Decode.Decoder Msg
-- dropDecoder =
--   Decode.at ["dataTransfer","files"] (Decode.oneOrMore GotNewUserImg File.decoder)

editor draggingImages oldpasswd newpasswd user =
    div [] [
         div [ class "vertical-flex-container"
             , hijackOn "dragenter" (D.succeed EditorDragEnter)
             , hijackOn "dragend" (D.succeed EditorDragLeave)
             , hijackOn "dragover" (D.succeed EditorDragEnter)
             , hijackOn "dragleave" (D.succeed EditorDragLeave)
             , style "background-color" (if draggingImages then "#880088" else "")
             ]
             [ h1 [ ] [text <| "User " ++ user.nickname ++ "'s settings" ]
             , img [ src user.img_location ] []
             , div [ id "img-helper"] [ text "If you want a new profile picture, drag and drop an image file here" ]
             , label [] [ text "Username: "
                        , input [ A.required True
                                , value user.username
                                , onInput SetUsername 
                                , type_ "text" ] []]
             , label [] [ text "Nickname: "
                        , input [ A.required True
                                , value user.nickname
                                , onInput SetNickname
                                , type_ "text"] []]
             , label [] [ text "New password: "
                        , input [ type_ "password"
                                , onInput SetNewpwd
                                , value newpasswd] []]
             , label [] [ text "Current password: "
                        , input [ A.required True
                                , type_ "password"
                                , value oldpasswd
                                , onInput SetOldpwd] []]]
        , button [ onClick (SubmitChangedUser oldpasswd newpasswd user) ] [ text "Submit changes!"]]
        
loginView loginstate =
    let actual_view = [label [for "username"] [text "Username"],
                       input [name "username", id "username", attribute "data-testid" "username-input-field", onInput ChangeUsername, onFocus LoginFocus ] [],
                       label [for "password"] [text "Password"],
                       input [name "password", attribute "data-testid" "password-input-field", id "password", type_ "password", onInput ChangePassword ] []
                           -- , label [] [text ("Loginstate: " ++ stateToText loginstate)]
                      ] in
    div [] (case loginstate of
                                  LoggedIn usr ->
                                      [p [attribute "data-testid" "welcome-user-label"] [ text "Welcome, "
                                                                                        , a [ href "/blog/usersettings" ]
                                                                                            [ text usr.nickname]]]
                                  LoggingIn username password ->
                                      (List.concat [actual_view,
                                                    [button [attribute "data-testid" "dologin", onClick DoLogIn] [text "Login!"]]])
                                  LoggedOut ->
                                      actual_view
                                  LoginFailed ->
                                      (List.concat [actual_view,
                                                    [button [onClick DoLogIn] [text "Login!"],
                                                     div [attribute "data-testid" "loginfailed"] [text "Login failed! Check username and password!"]]]))
