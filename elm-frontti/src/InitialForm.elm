module InitialForm exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Message exposing (..)

initialForm data =
    div []
        [ text "Welcome to your new Murja-blog. There doesn't seem to be any users in the db yet. Please provide the initial user- and rss-settings now. These can all be changed later in the ui."
        , div [ class "initial-form" ]
              [ h1 [] [ text "User settings"]
              , span [] []
              , h2 [] [ text "The initial admin user: " ]
              , span [] []                  
              , label [] [ text "Username (used when logging in): "
                         , input [ type_ "text"
                                 , value data.username
                                 , onInput SetInitialUsername ] []]
              , label [] [ text "Nickname (shown in the ui): "
                         , input [ type_ "text"
                                 , value data.nickname
                                 , onInput SetInitialNickname ] []]
              , label [] [ text "Password: "
                         , input [ type_ "password"
                                 , value data.password
                                 , onInput SetInitialPassword ] []]
              , span [] []
              , h1 [] [ text "Site settings"]
              , span [] []                  
              , label [] [ text "Domain (the dns name this site is reached by. This is used in session cookies. This should be left empty on dev.): "
                         , input [ type_ "text"
                                 , value data.domain
                                 , onInput SetInitialDomain ] []]
              , label [] [ text "Blog's title: "
                         , input [ type_ "text"
                                 , value data.blog_title
                                 , onInput SetInitialBlog_Title ] []]

              -- the stupid grid layout requires a few empty spans to lay h1 out into its own row
              , h1 [] [ text "RSS-feed settings"]
              , span [] []            
                  
              , label [] [ text "Rss-feed's title: "
                         , input [ type_ "text"
                                 , value data.rss_title
                                 , onInput SetInitialRss_Title ] []]
              , label [] [ text "Rss-feed's link: "
                         , input [ type_ "text"
                                 , value data.rss_link
                                 , onInput SetInitialRss_Link ] []]
              , label [] [ text "Rss-feed's description: "
                         , input [ type_ "text"
                                 , value <| Maybe.withDefault data.blog_title data.rss_description
                                 , onInput SetInitialRss_Description ] []]
              , label [] [ text "Rss-feed's language code: "
                         , input [ type_ "text"
                                 , value data.rss_lang
                                 , onInput SetInitialRss_Lang ] []]
              , label [] [ text "Rss-feed's listed email contact address: "
                         , input [ type_ "text"
                                 , value data.rss_email
                                 , onInput SetInitialRss_Email ] []]]
        , button
              [ onClick <| SaveInitialData data ]
              [ text "Start using murja"]]
