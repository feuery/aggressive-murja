module FeedView exposing (..)

import DateFormat as Df
import Feeds exposing (NewFeed)
import Time
import Message exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Button exposing (murja_button)
import UUID
import Article_view exposing (formatDateTime)

import Random

feed_item time_format zone item =
    li [] [ h1 [] [ text item.title]
          , h4 [] [ text (formatDateTime time_format zone item.pubdate)]
          , div [ class "feed-author"] [ text <| "By " ++ item.author]
          , div [ class "feed-item"
                , dangerouslySetInnerHTML item.description] []]

perFeedView settings zone fs new_feed_state = 
    div [] 
        [ ul [ class "feed-list" ]
              (List.map (\feed ->
                             li [ class "feed" ]
                             [ header [] [ text feed.name ]
                             , a [ href feed.url ] [ text feed.url ]
                             , ul [ class "feed-items" ]
                                 (feed.items
                                 |> List.sortBy (Time.posixToMillis << .pubdate)
                                 |> List.reverse
                                 |> List.map (feed_item settings.time_format zone))]) fs)
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
feeds feedReaderState settings zone fs new_feed  =
    let new_feed_state = Maybe.withDefault (NewFeed "" "") new_feed
    in
        div []
            [ span [] [ input [ id "feed_state_per_feed"
                              , type_ "checkbox"
                              , onClick SetPerFeedView
                              , checked <| feedReaderState == PerFeed] []
                      , label [ for "feed_state_per_feed"] [ text "Show rss per feed?"] ]
            
                    
            , case feedReaderState of
                  PerFeed -> perFeedView settings zone fs new_feed_state
                  SingleFeed -> div [] [ text "NotImplemented" ]]
