module Cropper.Image exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


-- MODEL


type alias Model =
    { imageUrl : String }


initialModel : Model
initialModel =
    { imageUrl = "/assets/30192_1600x1200-4-cute-cats.jpg" }



-- UPDATE


type Msg
    = SetImageUrl String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetImageUrl url ->
            ( { model | imageUrl = url }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ img [ src model.imageUrl ] [] ]
