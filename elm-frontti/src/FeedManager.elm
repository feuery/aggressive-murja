module FeedManager exposing (..)

import Message exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Button exposing (murja_button)

feedmanager feeds =
    ul [ class "feed-manager-container" ]
        (  feeds
        |> List.map (\f ->
                         li [ class "feed-manager" ]
                         [ span [] [ text <| f.name ++ " ("
                                   , a [ href f.url] [ text f.url ]
                                   , text ")"]
                         , murja_button [ onClick <| DeleteFeed f.id ]
                             [ text "Delete feed" ]]))
