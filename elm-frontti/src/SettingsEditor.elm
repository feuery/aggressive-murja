module SettingsEditor exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D

import Message exposing (..)

editor settings =
    div [ class "form-grid" ]
        [ label [ for "time-format"]
              [ text "Time format "]
        , input [ id "time-format"
                , onInput SetTimeFormat 
                , value settings.time_format] []
            
        , label [ for "title" ]
            [ text "Title" ]
        , input [ id "title"
                , onInput SetBlogTitle 
                , value settings.blog_title] []

        , label [ for "page_size" ]
            [ text "Page size (posts)"]
        , input [ id "page_size"
                , onInput SetPageSize 
                , value (String.fromInt settings.recent_post_count)
                , type_ "number"] []

        , label [ for "previously_label" ]
            [ text "Previously link label" ]
        , input [ id "previously"
                , onInput SetPreviouslyLabel
                , value settings.previously_label] []            

        , button [ onClick SaveSettings ]
            [ text "Save settings"]]
