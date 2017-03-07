module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, src, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, map, map4, at, string, list)


type alias Post =
    { title : String
    , subreddit : String
    , permalink : String
    , thumbnail : String
    }


type alias Listing =
    { posts : List Post
    }


type alias Model =
    { listing : Listing
    , subreddit : String
    }


type Msg
    = NewListing (Result Http.Error Listing)
    | NewSubreddit String
    | GoButtonPressed


init : String -> ( Model, Cmd Msg )
init path =
    ( Model (Listing []) "frontpage", getSubreddit "frontpage" )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewListing (Ok listing) ->
            ( { model | listing = listing }
            , Cmd.none
            )

        NewListing (Err _) ->
            ( model, Cmd.none )

        NewSubreddit subreddit ->
            ( { model | subreddit = subreddit }, Cmd.none )

        GoButtonPressed ->
            ( model, getSubreddit model.subreddit )


view : Model -> Html Msg
view model =
    div []
        [ titleView
        , subredditInputView model.subreddit
        , postsListView model.listing
        ]


postsListView : Listing -> Html Msg
postsListView listing =
    ul [] <| List.map postView listing.posts


postView : Post -> Html Msg
postView post =
    li []
        [ postThumbnailView post
        , postLinkView post
        ]


postThumbnailView : Post -> Html Msg
postThumbnailView post =
    case post.thumbnail of
        "self" ->
            span [] []

        "default" ->
            span [] []

        _ ->
            img [ src post.thumbnail ] []


postLinkView : Post -> Html Msg
postLinkView post =
    a [ href <| "http://www.reddit.com/" ++ post.permalink ] [ text post.title ]


titleView : Html Msg
titleView =
    h1 [] [ text "Reddit" ]


subredditInputView : String -> Html Msg
subredditInputView subreddit =
    div []
        [ input [ value subreddit, onInput NewSubreddit ] []
        , button [ onClick GoButtonPressed ] [ text "go" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


getSubreddit : String -> Cmd Msg
getSubreddit subreddit =
    let
        request =
            Http.request
                { method = "GET"
                , headers =
                    [ Http.header "Accept" "application/json"
                    ]
                , url = "https://api.reddit.com/r/" ++ subreddit
                , body = Http.emptyBody
                , expect = Http.expectJson (listingDecoder)
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send NewListing request


listingDecoder : Decoder Listing
listingDecoder =
    Json.Decode.map Listing
        (at [ "data", "children" ] <| list postDecoder)


postDecoder : Decoder Post
postDecoder =
    map4 Post
        (at [ "data", "title" ] string)
        (at [ "data", "subreddit" ] string)
        (at [ "data", "permalink" ] string)
        (at [ "data", "thumbnail" ] string)
