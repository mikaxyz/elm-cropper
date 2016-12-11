module Sandbox exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


main : Program Never Model Msg
main =
    Html.program
        { init = ( "I am sand. Sand box.", Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    String



-- UPDATE


type Msg
    = Reverse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reverse ->
            ( String.reverse model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    h1 [ style [ ( "cursor", "pointer" ), ( "user-select", "none" ) ], onClick Reverse ] [ text model ]
