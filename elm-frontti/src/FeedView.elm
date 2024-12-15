module FeedView exposing (..)

import DateFormat as Df
import Feeds exposing (..)
import Dict 
import Time
import Message exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, on) 
import Button exposing (murja_button)
import UUID
import Article_view exposing (formatDateTime)
import FeedManager exposing (feedmanager)
import Tab exposing (TabEntry, tabs)
import Random

feed_item loginstate settings zone item =
    let time_format = settings.time_format
        article = itemToArticle item in
    li [ class "feed-item" ]
       ( case item.feed_id of
             Just feed_id ->
                 [ div [] [ murja_button [ onClick <| SelectExcerpt item.id ] [ text "Write a post about this"]]
                 , node "dialog" [ id <| "excerpt-dialog-" ++ (UUID.toString item.id) ]
                     (let textarea_id = "excerpt-dialog-" ++ (UUID.toString item.id) ++ "-textarea" in 
                     [ div [ class "dialog" ]
                           [ header [ class "previouslyHeader" ] [ button [ onClick ClosePreviousPostsModel ] [ text "X"]]
                           , h3 [] [ text "Select an excerpt"]
                           , textarea [ rows 30
                                      , cols 60
                                      , id textarea_id ] [ text item.description ]
                           , murja_button [ onClick <| CreateExcerptPost textarea_id item.id ] [ text "Create a post" ]]])
                 , Article_view.articleView settings loginstate zone article ]
             Nothing ->
                 [ text "Unknown feed" ])

correctlySortedFeedItemList loginstate settings zone items = 
    (  items
    |> List.sortBy (Time.posixToMillis << .pubdate)
    |> List.reverse
    |> List.map (feed_item loginstate settings zone))

-- fs = feeds, elm sucks balls at shadowing
perFeedView loginstate settings zone fs new_feed_state = 
    ul [ class "feed-list" ]
        (List.map (\feed ->
                       li [ class "feed" ]
                       [ header [] [ text <| feed.name ++ " (" ++ (String.fromInt (List.length feed.items)) ++ ")" ]
                       , a [ href feed.url ] [ text feed.url ]
                       , ul [ class "feed-items" ]
                           (correctlySortedFeedItemList loginstate settings zone <| List.map (\i -> { i | feed_id = Just feed.id}) feed.items)]) fs)
            
singleFeedView loginstate settings zone fs =
    let final_feed = List.concatMap (\feed -> List.map (\item -> { item | feed_id = Just feed.id}) feed.items) fs in
    div []
        [ header [] [ text <| (String.fromInt (List.length final_feed)) ++ " unread articles"  ]
        , ul [ class "feed-items" ]
            (correctlySortedFeedItemList loginstate settings zone final_feed)]
              

readerState_str state =
    case state of
        PerFeed -> "PerFeed"
        SingleFeed -> "SingleFeed"
        FeedManager -> "FeedManager"
                             
feeds feedReaderState loginstate show_archived settings zone fs new_feed metadata =
    let new_feed_state = Maybe.withDefault (NewFeed "" "") new_feed
    in
        div [ id "feeds" ] 
            [ label [] [ input [ type_ "checkbox"
                               , checked show_archived
                               , onClick <| ShowArchivedFeedItems (not show_archived)] []
                       , text "Show read items"]
            , tabs "rss-feed-tab" (readerState_str feedReaderState) Nothing
                  (Dict.fromList [ ("PerFeed", TabEntry "Group by feed" "" (perFeedView loginstate settings zone fs new_feed_state) Nothing ["*"])
                                 , ("SingleFeed", TabEntry "Show all in a feed" "" (singleFeedView loginstate settings zone fs) Nothing ["*"])
                                 , ("FeedManager", TabEntry "Manage feeds" "" (feedmanager settings.time_format zone fs) Nothing ["*"])])
                  
            , h3 [] [ text "Add new feed?"]
            , label [ for "name" ] [ text "Feed name" ]
            , input [ id "name"
                    , onInput SetFeedName
                    , value new_feed_state.name
                    , type_ "text"] []

            , label [ for "url" ] [ text "Url to feed" ]
            , input [ id "url"
                    , onInput SetFeedUrl
                    , value new_feed_state.url
                    , type_ "text"] []
            , murja_button [ onClick (AddFeed new_feed_state)] [ text "Add a feed"]
            , case metadata of
                  Just meta -> 
                      details [] [ summary [] [ text "Latest feed updates at: "]
                                 , ul []
                                     (  meta.last_update_timestamps
                                     |> List.map (\timestamp -> li [] [ text timestamp]))]
                  Nothing -> text "Metadata didn't load"]
