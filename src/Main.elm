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


cropSmall : { height : Int, width : Int }
cropSmall =
    { width = 240
    , height = 160
    }


cropMedium : { height : Int, width : Int }
cropMedium =
    { width = 640
    , height = 480
    }


cropFacebook : { height : Int, width : Int }
cropFacebook =
    { width = 820
    , height = 312
    }


view : Model -> Html Msg
view model =
    div []
        [ h4 [] [ text model.name ]
        , div [ class "section" ] [ Html.map CropperMsg <| Cropper.view model.cropperModel ]
        , zoomWidget model
        ]


zoomWidget : Model -> Html Msg
zoomWidget model =
    div [ class "cropper__controls" ]
        [ h4 [] [ text <| "ZOOM: " ++ (toString model.cropperModel.image.zoom) ]
        , a [ class "button", onClick <| CropperMsg <| Cropper.SetImage newImg ] [ text "*" ]
        , a [ class "button", onClick <| CropperMsg <| Cropper.CropTo cropSmall ] [ text "S" ]
        , a [ class "button", onClick <| CropperMsg <| Cropper.CropTo cropMedium ] [ text "M" ]
        , a [ class "button", onClick <| CropperMsg <| Cropper.CropTo cropFacebook ] [ text "FB" ]
        , a [ class "button", onClick <| ExportImage ] [ text "!" ]
        , input [ style [ ( "width", "50%" ) ], onInput Zoom, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (toString model.cropperModel.image.zoom) ] []
        , input [ style [ ( "width", "50%" ) ], onInput PivotX, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (toString model.cropperModel.image.pivot.x) ] []
        , input [ style [ ( "width", "50%" ) ], onInput PivotY, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (toString model.cropperModel.image.pivot.y) ] []
        ]
