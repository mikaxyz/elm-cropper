port module Main exposing (..)

import Util.Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Image.Cropper as Cropper


-- PORTS


type alias ImageData =
    { url : String
    , width : Int
    , height : Int
    }


port cropperWithImage : (ImageData -> msg) -> Sub msg


type alias CropData =
    { url : String
    , size :
        { width : Int
        , height : Int
        }
    , crop :
        { width : Int
        , height : Int
        }
    , resized :
        { width : Int
        , height : Int
        }
    , origin :
        { x : Int
        , y : Int
        }
    }


createCropData : Model -> CropData
createCropData model =
    let
        size =
            Cropper.imageSize model.cropperModel.image

        origin =
            Cropper.cropOrigin model.cropperModel.image
    in
        { url = model.cropperModel.image.imageUrl
        , size =
            { width = model.cropperModel.image.naturalSize.width
            , height = model.cropperModel.image.naturalSize.height
            }
        , crop =
            { width = model.cropperModel.image.crop.width
            , height = model.cropperModel.image.crop.height
            }
        , resized =
            { width = round size.x
            , height = round size.y
            }
        , origin =
            { x = round origin.x
            , y = round origin.y
            }
        }


port imageCropped : CropData -> Cmd msg



--port imageCropped : Model -> Cmd msg


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
    { cropperModel : Cropper.Model
    }


initialModel : Model
initialModel =
    { cropperModel = Cropper.initialModel
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = Zoom String
    | ExportImage
    | SetImageData ImageData
    | PivotX String
    | PivotY String
    | CropperMsg Cropper.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExportImage ->
            ( model, imageCropped (createCropData model) )

        --            ( model, imageCropped model )
        SetImageData data ->
            let
                _ =
                    Debug.log "Teeeest" data
            in
                update (CropperMsg <| (Cropper.SetImage data)) model

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
        , cropperWithImage SetImageData
        ]



-- VIEW


newImg : { height : Int, url : String, width : Int }
newImg =
    { url = "/assets/tv-digital-art-test-pattern-1920x1080-68386.jpg", width = 1920, height = 1080 }


view : Model -> Html Msg
view model =
    div []
        [ header []
            [ h2 [] [ text "Elm Image Crop Example" ]
            , p []
                [ text "Here is an image of some cats. You can use the sliders below to zoom or position it. Try dragging the image. If cats are not your thing: "
                , button [ onClick <| CropperMsg <| Cropper.SetImage newImg ] [ text "Try another image" ]
                ]
            , p []
                [ text "Crop to size:"
                , button [ onClick <| CropperMsg <| Cropper.CropTo { width = 240, height = 160 } ] [ text "240×160" ]
                , button [ onClick <| CropperMsg <| Cropper.CropTo { width = 640, height = 480 } ] [ text "640×480" ]
                , button [ onClick <| CropperMsg <| Cropper.CropTo { width = 820, height = 312 } ] [ text "820×312" ]
                ]
            ]
        , div [ class "info-bar", style [ ( "max-width", toString model.cropperModel.image.crop.width ++ "px" ) ] ] (sourceInfoItems model.cropperModel)
        , div [] [ Html.map CropperMsg <| Cropper.view model.cropperModel ]
        , div [ class "info-bar", style [ ( "max-width", toString model.cropperModel.image.crop.width ++ "px" ) ] ] (cropInfoItems model.cropperModel)
        , zoomWidget model
        ]


sourceInfoItems : Cropper.Model -> List (Html Msg)
sourceInfoItems model =
    [ span [] [ "W: " ++ toString model.image.naturalSize.width |> text ]
    , span [] [ "H: " ++ toString model.image.naturalSize.height |> text ]
    , span [ class "fill" ] [ "SRC: " ++ model.image.imageUrl |> text ]
    ]


showRound : Int -> Float -> String
showRound d value =
    let
        f =
            (round (value * toFloat (10 ^ d))) % (10 ^ d)
    in
        toString (floor value) ++ "." ++ (String.padLeft d '0' <| toString f)


cropInfoItems : Cropper.Model -> List (Html Msg)
cropInfoItems model =
    [ span [] [ "W: " ++ showRound 2 (Cropper.imageSize model.image).x |> text ]
    , span [] [ "H: " ++ showRound 2 (Cropper.imageSize model.image).y |> text ]
    , span [] [ "X: " ++ toString (floor (Cropper.cropOrigin model.image).x) |> text ]
    , span [] [ "Y: " ++ toString (floor (Cropper.cropOrigin model.image).y) |> text ]
    ]


zoomWidget : Model -> Html Msg
zoomWidget model =
    div [ class "controls" ]
        [ p [ class "controls__row" ]
            [ label [] [ text "Z" ]
            , input [ onInput Zoom, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (toString model.cropperModel.image.zoom) ] []
            , span [] [ text <| showRound 4 model.cropperModel.image.zoom ]
            ]
        , p [ class "controls__row" ]
            [ label [] [ text "X" ]
            , input [ onInput PivotX, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (toString model.cropperModel.image.pivot.x) ] []
            , label [] [ text "Y" ]
            , input [ onInput PivotY, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (toString model.cropperModel.image.pivot.y) ] []
            ]
        , button [ class "controls__button", onClick <| ExportImage ] [ text "Crop" ]
        ]
