module Logviewer exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Regex
import List.Extra
import Logs exposing (..)

import Message exposing (..)

dropMaybe: (List (Maybe a)) -> List a
dropMaybe lst =
    List.foldr (\val -> (\acc ->
                            case val of
                                Just a -> a :: acc
                                Nothing -> acc)) [] lst

log_to_list log
    = li [] [ text log.row ]

listify group 
    = let is_ungrouped = group.name == "Ungrouped"
      in 
      details []
      [ summary [ class "loggroup-summary" ] [
             span [ class "loggroup-summary-container" ]
                 [ text <| group.name ++ " (" ++ (String.fromInt (List.length group.members)) ++ ")" ++ (if group.sound_alarm then " ALERT!  " else "")
                 , if not is_ungrouped then
                       button [ onClick <| DeleteLogGroup group.name ] [ text "Delete log group" ]
                   else div [] []
                 , if not is_ungrouped then
                       label [] [ input [ type_ "checkbox"
                                        , checked group.alarmy
                                        , onClick (SetLogAlarmy group (not group.alarmy))] []
                                , text "Alarm"]
                   else
                       div [] []]]
      , ul []
          (List.map log_to_list group.members)]

parseRegex: Group -> Maybe ParsedGroup
parseRegex r =
    case Regex.fromString r.name of
        Just rr -> Just (ParsedGroup r.name rr r.alarmy r.sound_alarm r.members)
        Nothing -> Nothing

tab: List Log -> List Group -> String -> Html Msg
tab logs groups edited_group =
    let regexes = groups
                  |> List.map parseRegex
                  |> dropMaybe 
        grouped = regexes
                  |> List.map (\regex ->
                                   let members = List.filter (\l -> Regex.contains regex.regex l.row) logs
                                   in 
                                       { regex | members = members})
                  |> List.map listify
        last_group = logs
                   |> List.filter (\l -> regexes
                                      |> List.filter (\r -> Regex.contains r.regex l.row)
                                      |> (==) [])
                   |> ParsedGroup "Ungrouped" Regex.never False False 
                   |> listify 
    in 
    div []
        [ h3 [] [ text "Add a new group" ]
        , label [] [ text "Grouping regexp "
                   , input [ type_ "text"
                           , value edited_group
                           , onInput EditGroupRegexp] []]
        , button [ type_ "button"
                 , onClick <| SaveLogGroup edited_group] [ text "Add group"]
        , h3 [] [ text "Logs: "]
        , div [] grouped
        , last_group]
