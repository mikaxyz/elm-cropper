module Main exposing (..)

import Util.Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
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
    , cropperModel : Cropper.Model
    }


initialModel : Model
initialModel =
    { name = "I am sand. Sand box."
    , cropperModel = Cropper.initialModel
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = Zoom String
    | PivotX String
    | PivotY String
    | CropperMsg Cropper.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Zoom zoom ->
            update (CropperMsg <| Cropper.SetZoom (Result.withDefault 0 (String.toFloat zoom))) model

        PivotX x ->
            update (CropperMsg <| Cropper.SetPivotX (Result.withDefault 0 (String.toFloat x))) model

        PivotY y ->
            update (CropperMsg <| Cropper.SetPivotY (Result.withDefault 0 (String.toFloat y))) model

        CropperMsg subMsg ->
            let
                _ =
                    debugOff "CropperMsg" subMsg

                ( updatedSubModel, subCmd ) =
                    Cropper.update subMsg model.cropperModel
            in
                ( { model | cropperModel = updatedSubModel }, Cmd.map CropperMsg subCmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map CropperMsg (Cropper.subscriptions model.cropperModel)
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h4 [] [ text model.name ]
        , div [ class "section" ] [ Html.map CropperMsg <| Cropper.view model.cropperModel ]
          --        , a [ class "button", onClick <| CropperMsg <| Cropper.SetImageUrl "https://i.ytimg.com/vi/opKg3fyqWt4/hqdefault.jpg" ] [ text "Make pup" ]
        , zoomWidget model
        ]


zoomWidget : Model -> Html Msg
zoomWidget model =
    div [ class "cropper__controls" ]
        [ h4 [] [ text <| "ZOOM: " ++ (toString model.cropperModel.zoom) ]
        , input [ style [ ( "width", "50%" ) ], onInput Zoom, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (toString model.cropperModel.zoom) ] []
        , input [ style [ ( "width", "50%" ) ], onInput PivotX, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (toString model.cropperModel.pivot.x) ] [ text "asdas" ]
        , input [ style [ ( "width", "50%" ) ], onInput PivotY, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (toString model.cropperModel.pivot.y) ] []
        ]
