module Tab exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Message exposing (..)

type alias TabEntry =
    { title: String
    , component: Html Msg
    , onclick: Maybe Msg
    , permissions: List String}

-- tabs: String -> String -> Dict String TabEntry -> Html Msg
tabs tab_id selected_tab usr tabentries =
    let tab_ids = ( tabentries
                  |> Dict.filter (\k -> \entry -> case usr of
                                                      Just user -> ((  entry.permissions
                                                                    |> Set.fromList
                                                                    |> Set.intersect (Set.fromList user.permissions)
                                                                    |> Set.isEmpty
                                                                    |> not)
                                                                   ||
                                                                        (List.member "*" entry.permissions))
                                                      Nothing -> (List.member "*" entry.permissions))
                  |> Dict.keys)
        selected_tab_component = Dict.get selected_tab tabentries 
    in 
    div [ class "tabs"
        , id tab_id ]
        [ ul [ class "tab-headers" ]
              (  tab_ids
              |> List.map (\id ->
                               let entry_ = Dict.get id tabentries in
                               case entry_ of
                                   Just entry -> 
                                       li [ class (if selected_tab == id then "tab-header tab-selected" else "tab-header")
                                          , onClick (case entry.onclick of
                                                         Just oc -> oc
                                                         Nothing -> SelectTab tab_id id)] [ text entry.title]
                                   Nothing -> li [] [ text "Unknown tab" ]))
        , case selected_tab_component of
              Just c -> c.component
              Nothing -> div [] [ text <| "Invalid selected tab key " ++ (Debug.toString selected_tab)]]
