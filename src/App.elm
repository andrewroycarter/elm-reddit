module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (int, string, float, nullable, Decoder)
import Json.Decode.Pipeline exposing (decode, required)


type alias Post =
    { title : String
    , subreddit : String
    }


type alias Model =
    { posts : List Post }


init : String -> ( Model, Cmd Msg )
init path =
    ( Model [], Cmd.none )


type Msg
    = NoOp
    | NewListing (Result Http.Error Post)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ titleView
        ]


postsListView : Model -> Html Msg
postsListView model =
    ul [] <| List.map postView model.posts


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
        url =
            "api.reddit.com"
        request =
            Http.get url postDecoder
    in
        Http.send NewListing request


listingDecoder : Decoder List Post 
listingDecoder =

postDecoder : Decoder Post
postDecoder =
    decode Post
        |> required "title" string "-"
        |> required "subreddit" string "-
