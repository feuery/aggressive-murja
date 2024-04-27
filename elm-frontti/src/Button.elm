module Button exposing (murja_button)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

murja_button params contents =
    button (List.append params [ class "murja-button" ])
        contents
