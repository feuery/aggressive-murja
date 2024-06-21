module FeedManager exposing (..)

import Message exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Button exposing (murja_button)
import Article_view exposing (formatDateTime)
import Time 

feedmanager time_format zone feeds =
    table [ class "feed-manager-container" ]
        (  feeds
        |> List.map (\f ->
                         let last_updated = (  f.items
                                            |> List.map (Time.posixToMillis << .pubdate)
                                            |> List.maximum) in
                         ( last_updated
                         , f))
        |> List.sortBy ((\last_updated ->
                             case last_updated of
                                 Just d -> d
                                 Nothing -> 9999) << Tuple.first)
        |> List.reverse
        |> List.map (\tuple ->
                         let (last_updated, f) = tuple in
                         tr [ ]
                         [ td [] [ text <| f.name ++ " ("
                                 , a [ href f.url] [ text f.url ]
                                 , text ")"]
                         , td [] [ text <| "Last updated at " ++ (  last_updated
                                                                 |> Maybe.map Time.millisToPosix
                                                                 |> (Maybe.map <| formatDateTime time_format zone)
                                                                 |> Maybe.withDefault "never")]
                         , td []
                              [ murja_button [ onClick <| DeleteFeed f.id ]
                                    [ text "Delete feed" ]]]))
