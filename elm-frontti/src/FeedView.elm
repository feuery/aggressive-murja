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
import Tab exposing (tabs)

import Random

feed_item time_format zone item =
    li [] [ h1 [] [ a [ href item.link ] [ text item.title] ]
          , (if item.title == "" then 
                  h4 [] [ a [ href item.link] [ text (formatDateTime time_format zone item.pubdate) ]]
             else
                 h4 [] [ text (formatDateTime time_format zone item.pubdate)])
          , div [ class "feed-author"] [ text <| "By " ++ item.author]
          , div [ class "feed-item"
                , dangerouslySetInnerHTML item.description] []]

correctlySortedFeedItemList settings zone items = 
    (  items
    |> List.sortBy (Time.posixToMillis << .pubdate)
    |> List.reverse
    |> List.map (feed_item settings.time_format zone))

-- fs = feeds, elm sucks balls at shadowing
perFeedView settings zone fs new_feed_state = 
    ul [ class "feed-list" ]
        (List.map (\feed ->
                       li [ class "feed" ]
                       [ header [] [ text feed.name ]
                       , a [ href feed.url ] [ text feed.url ]
                       , ul [ class "feed-items" ]
                           (correctlySortedFeedItemList settings zone feed.items)]) fs)
            
singleFeedView settings zone fs =
    let feed = List.concatMap .items fs in 
    ul [ class "feed-items" ]
        (correctlySortedFeedItemList settings zone feed)
              

readerState_str state =
    case state of
        PerFeed -> "PerFeed"
        SingleFeed -> "SingleFeed"
                             
feeds feedReaderState settings zone fs new_feed  =
    let new_feed_state = Maybe.withDefault (NewFeed "" "") new_feed
    in
        div [ id "feeds" ] 
            [ tabs "rss-feed-tab" (readerState_str feedReaderState)
                  (Dict.fromList [ ("PerFeed", "Group by feed")
                                 , ("SingleFeed", "Show all in a feed")])
                  (Dict.fromList [ ("PerFeed", perFeedView settings zone fs new_feed_state)
                                 , ("SingleFeed", singleFeedView settings zone fs)])
                  
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
            , murja_button [ onClick (AddFeed new_feed_state)] [ text "Add a feed"]]
