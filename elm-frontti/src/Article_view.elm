module Article_view exposing (..)

import DateFormat as Df
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import User
import Message exposing (..)
import Settings
import Time
import Article


formatDateTime formatString zone posixTime =
    Df.format formatString zone posixTime

articleView settings loginstate zone the_actual_post =
    let versions = Maybe.withDefault [] the_actual_post.versions
        url = case the_actual_post.source of
                  Article.Rss link -> link 
                  Article.Murja -> ("/blog/post/" ++ String.fromInt the_actual_post.id)
    in
        div [class "post"] [ a [ href url ] [ text the_actual_post.title ]
                           , div [class "meta"] (List.append [ User.user_avatar the_actual_post.creator
                                                             , p [] [text ("By " ++ the_actual_post.creator.nickname)]
                                                             , case the_actual_post.created_at of
                                                                   Just writing_time ->
                                                                       p [] [text ("Written at " ++ (formatDateTime settings.time_format zone writing_time))]
                                                                   Nothing ->
                                                                       p [] [text ("No idea when it's written")]]
                                                     (let post_id = the_actual_post.id in
                                                      (List.map (\version -> a [ href ("/blog/post/" ++ String.fromInt post_id ++ "/version/" ++ String.fromInt version) ] [ text ((String.fromInt version) ++ ", ")]) versions)))
                             
                           , (let post_id = the_actual_post.id in
                              case loginstate of
                                  LoggedIn _ -> case the_actual_post.source of
                                                    Article.Murja -> a [ href ("/blog/post/edit/" ++ String.fromInt post_id)
                                                                       , attribute "data-testid" "edit-post-btn"
                                                                       , onClick (OpenPostEditor post_id)] [text "Edit this post"]
                                                    Article.Rss _ -> div [] []
                                  _ -> div [] [])
                                                    
                           , article [ class "content"
                                     , dangerouslySetInnerHTML the_actual_post.content] []
                           , div [ class "previously" ]
                               (  the_actual_post.previously
                               |> List.map (\prev -> span [] [ a [ href ("/blog/post/" ++ (String.fromInt prev.id))
                                                                 , title prev.title]
                                                                   [ text settings.previously_label ]
                                                             , text ", "]))
                           , let tags = ( the_actual_post.tags
                                        |> List.filter ((/=) ""))
                             in
                                 if not (List.isEmpty tags) then 
                                     div [ class "tags" ] ( tags 
                                                          |> List.map ( \tag -> span [] [ a [ href ("/blog/tags/" ++ tag)
                                                                                            , class "tag" ] [text tag]
                                                                                        , text ", "]))
                                 else div [] []]
