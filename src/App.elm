module App exposing (..)

import Html exposing (..)
import Http
import Json.Decode exposing (Decoder, map, map2, at, string, list)


type alias Post =
    { title : String
    , subreddit : String
    }


type alias Listing =
    { posts : List Post
    }


type alias Model =
    { listing : Listing }


type Msg
    = NewListing (Result Http.Error Listing)


init : String -> ( Model, Cmd Msg )
init path =
    ( Model <| Listing [], getFrontPage )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewListing (Ok listing) ->
            ( { model | listing = listing }
            , Cmd.none
            )

        NewListing (Err _) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ titleView
        , postsListView model
        ]


postsListView : Model -> Html Msg
postsListView model =
    ul [] <| List.map postView model.listing.posts


postView : Post -> Html Msg
postView post =
    li [] [ text post.title ]


titleView : Html Msg
titleView =
    h1 [] [ text "Reddit" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


getFrontPage : Cmd Msg
getFrontPage =
    let
        request =
            Http.request
                { method = "GET"
                , headers = []
                , url = "http://api.reddit.com/.json"
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
    map2 Post
        (at [ "data", "title" ] string)
        (at [ "data", "subreddit" ] string)
