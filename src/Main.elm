module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Cropper.Image as Cropper


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { name : String
    , zoom : String
    , cropperModel : Cropper.Model
    }


initialModel : Model
initialModel =
    { name = "I am sand. Sand box."
    , zoom = "1.0"
    , cropperModel = Cropper.initialModel
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = Set String
    | Zoom String
    | CropperMsg Cropper.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Set to ->
            ( model, Cmd.none )

        Zoom zoom ->
            update (CropperMsg <| Cropper.SetZoom (Result.withDefault 0 (String.toFloat zoom))) { model | zoom = zoom }

        CropperMsg subMsg ->
            let
                ( updatedSubModel, subCmd ) =
                    Cropper.update subMsg model.cropperModel
            in
                ( { model | cropperModel = updatedSubModel }, Cmd.map CropperMsg subCmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h4 [] [ text model.name ]
        , div [ class "section" ] [ Html.map CropperMsg <| Cropper.view model.cropperModel ]
        , a [ class "button", onClick <| CropperMsg <| Cropper.SetImageUrl "https://i.ytimg.com/vi/opKg3fyqWt4/hqdefault.jpg" ] [ text "Make pup" ]
        , zoomWidget model
        ]


zoomWidget : Model -> Html Msg
zoomWidget model =
    div []
        [ h4 [] [ text <| "ZOOM: " ++ model.zoom ]
        , input [ style [ ( "width", "50%" ) ], onInput Zoom, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value model.zoom ] []
        ]
