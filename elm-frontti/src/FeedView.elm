module FeedView exposing (..)

import DateFormat as Df
import Feeds exposing (NewFeed)
import Message exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Button exposing (murja_button)
import UUID

import Random

feeds fs new_feed  =
    let new_feed_state = Maybe.withDefault (NewFeed "" "") new_feed
    in 
    div [] 
        [ ul [ class "feed-list" ]
              (List.map (\feed ->
                             li [ class "feed" ]
                             [ header [] [ text feed.name ]
                             , a [ href feed.url ] [ text feed.url ]]) fs)
        , h3 [] [ text "Add new feed?"]
        , div []
            [ label [ for "name" ] [ text "Feed name" ]
            , input [ id "name"
                    , onInput SetFeedName
                    , value new_feed_state.name
                    , type_ "text"] []

            , label [ for "url" ] [ text "Url to feed" ]
            , input [ id "url"
                    , onInput SetFeedUrl
                    , value new_feed_state.url
                    , type_ "text"] []
            , murja_button [ onClick (AddFeed new_feed_state)] [ text "Add a feed"]]]
                      
