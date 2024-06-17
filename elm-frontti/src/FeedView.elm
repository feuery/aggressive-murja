module FeedView exposing (..)

import DateFormat as Df
import Feeds exposing (NewFeed)
import Dict 
import Time
import Message exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Button exposing (murja_button)
import UUID
import Article_view exposing (formatDateTime)
import FeedManager exposing (feedmanager)
import Tab exposing (tabs)

import Random

feed_item time_format zone item =
    case item.feed_id of
        Just feed_id ->
            li [] [ h1 [] [ a [ href item.link ] [ text item.title] ]
                  , (if item.title == "" then 
                         h4 [] [ a [ href item.link] [ text (formatDateTime time_format zone item.pubdate) ]]
                     else
                         h4 [] [ text (formatDateTime time_format zone item.pubdate)])
                  , div [ class "feed-read"] [
                         let is_read = item.is_read in
                         button [ onClick <| ReadFeedItem feed_id item.id (not is_read) ] [ text "Mark as read"]]
                  , div [ class "feed-author"] [ text <| "By " ++ item.author]
                  , div [ class "feed-item"
                        -- this is a bit moronic xss vuln, I should research how others have sanitized the html from rss/atom feeds 
                        , dangerouslySetInnerHTML item.description] []]
        Nothing ->
            li [] [ text "Unknown feed" ]

correctlySortedFeedItemList show_archived settings zone items = 
    (  items
    |> List.sortBy (Time.posixToMillis << .pubdate)
    |> List.reverse
    |> List.filter (\item -> (not item.is_read) || show_archived)
    |> List.map (feed_item settings.time_format zone))

-- fs = feeds, elm sucks balls at shadowing
perFeedView show_archived settings zone fs new_feed_state = 
    ul [ class "feed-list" ]
        (List.map (\feed ->
                       li [ class "feed" ]
                       [ header [] [ text feed.name ]
                       , a [ href feed.url ] [ text feed.url ]
                       , ul [ class "feed-items" ]
                           (correctlySortedFeedItemList show_archived settings zone <| List.map (\i -> { i | feed_id = Just feed.id}) feed.items)]) fs)
            
singleFeedView show_archived settings zone fs =
    let final_feed = List.concatMap (\feed -> List.map (\item -> { item | feed_id = Just feed.id}) feed.items) fs in 
    ul [ class "feed-items" ]
        (correctlySortedFeedItemList show_archived settings zone final_feed)
              

readerState_str state =
    case state of
        PerFeed -> "PerFeed"
        SingleFeed -> "SingleFeed"
        FeedManager -> "FeedManager"
                             
feeds feedReaderState show_archived settings zone fs new_feed metadata =
    let new_feed_state = Maybe.withDefault (NewFeed "" "") new_feed
    in
        div [ id "feeds" ] 
            [ label [] [ input [ type_ "checkbox"
                               , checked show_archived
                               , onClick <| ShowArchivedFeedItems (not show_archived)] []
                       , text "Show read items"]
            , tabs "rss-feed-tab" (readerState_str feedReaderState)
                  (Dict.fromList [ ("PerFeed", "Group by feed")
                                 , ("SingleFeed", "Show all in a feed")
                                 , ("FeedManager", "Manage feeds")])
                  (Dict.fromList [ ("PerFeed", perFeedView show_archived settings zone fs new_feed_state)
                                 , ("SingleFeed", singleFeedView show_archived settings zone fs)
                                 , ("FeedManager", feedmanager fs)])
                  
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
