module User exposing (..)

import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (..)

import Message exposing (..)
import Article exposing (decodeApply)
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Decode.Extra as Extra
import Json.Encode as Json
import File exposing (File)


    -- {
    --       "nickname": "Feuer",
    --             "img_location": "https://feuerx.net/etc/feuer.jpeg",
    --             "userid": 1,
    --             "primary-group-name": "Admins",
    --             "permissions": [
    --                  "edit-self",
    --                          "comment-post",
    --                          "edit-user",
    --                          "create-comment",
    --                          "edit-post",
    --                          "delete-post",
    --                          "create-post",
    --                          "create-page",
    --                          "delete-comment",
    --                          "delete-user",
    --                          "can-import",
    --                          "edit-comment"
    --                        ]


nicknameDecoder = Decode.field "nickname" Decode.string
imgDecoder = Decode.field "img_location" Decode.string
group_name_decoder = Decode.field "primary-group-name" Decode.string
permissionsDecoder = Decode.field "permissions" (Decode.list Decode.string)
usernameDecoder = Decode.field "username" Decode.string
idDecoder = Decode.field "userid" Decode.int
                     
-- |> == clojure's ->>
userDecoder : Decoder LoginUser
userDecoder =
    Decode.succeed LoginUser
        |> decodeApply nicknameDecoder
        |> decodeApply usernameDecoder
        |> decodeApply imgDecoder
        |> decodeApply group_name_decoder
        |> decodeApply permissionsDecoder
        |> decodeApply idDecoder
    
stateToText state =
    case state of
        LoggedIn _ -> "LoggedIn"
        LoggingIn _ _ -> "LoggingIn"
        LoggedOut -> "LoggedOut"
        LoginFailed -> "LoginFailed"

user_avatar creator = img [class "user_avatar", src creator.img_location] []

type alias UserLoggingIn =
    { username : String
    , password : String}

encodeLoggingIn user =
    Json.object
        [ ("username", Json.string user.username)
        , ("password", Json.string user.password)]

encodeEditorUser : LoginUser -> String -> String-> Json.Value
encodeEditorUser usr oldpasswd newpasswd =
    Json.object
        [ ("nickname", Json.string usr.nickname)
        , ("username", Json.string usr.username)
        , ("img_location", Json.string usr.img_location)
        , ("id", Json.int usr.id) -- unique and immutable key, needed because UserEditor.editor lets user change all the other values
        , ("old-password", Json.string oldpasswd)
        , ("new-password", Json.string newpasswd)]                   
    
             
