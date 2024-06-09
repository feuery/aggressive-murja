module Tab exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Message exposing (..)

-- tabs: String -> comparable -> Dict comparable String -> Dict comparable (Html msg) -> Html msg
tabs tab_id selected_tab_key titles tabkey_to_page =
    div [ class "tabs"
        , id tab_id]
        [ ul [ class "tab-headers" ]
              (  titles
              |> Dict.map
                     (\k -> \title -> li [ class <| if selected_tab_key == k then "tab-header tab-selected" else "tab-header"
                                         , onClick (SelectTab tab_id k)] [ text title])
              |> Dict.values)
        ,  (  Dict.get selected_tab_key tabkey_to_page
           |> Maybe.withDefault (div [] [ text <| "Invalid selected tab key " ++ (Debug.toString selected_tab_key)]))]
              
