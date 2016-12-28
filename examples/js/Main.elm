port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Image.Cropper as Cropper


-- PORTS


type alias ImageData =
    { url : String
    , crop :
        { width : Int
        , height : Int
        }
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
    case (Cropper.getImageData model.cropperModel) of
        Nothing ->
            CropData "" { width = 0, height = 0 } { width = 0, height = 0 } { width = 0, height = 0 } { x = 0, y = 0 }

        Just image ->
            let
                size =
                    Cropper.imageSize model.cropperModel

                origin =
                    Cropper.cropOrigin model.cropperModel
            in
                { url = image.imageUrl
                , size =
                    { width = image.naturalSize.width
                    , height = image.naturalSize.height
                    }
                , crop =
                    { width = image.crop.width
                    , height = image.crop.height
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

        SetImageData data ->
            update (CropperMsg <| (Cropper.SetImage data)) model

        Zoom zoom ->
            update (CropperMsg <| Cropper.SetZoom (Result.withDefault 0 (String.toFloat zoom))) model

        PivotX x ->
            update (CropperMsg <| Cropper.SetPivotX (Result.withDefault 0 (String.toFloat x))) model

        PivotY y ->
            update (CropperMsg <| Cropper.SetPivotY (Result.withDefault 0 (String.toFloat y))) model

        CropperMsg subMsg ->
            let
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


catImg : ImageData
catImg =
    { url = "/assets/30192_1600x1200-4-cute-cats.jpg"
    , crop = { width = 320, height = 540 }
    }


testImg : ImageData
testImg =
    { url = "/assets/tv-digital-art-test-pattern-1920x1080-68386.jpg"
    , crop = { width = 820, height = 312 }
    }


view : Model -> Html Msg
view model =
    let
        image =
            Cropper.getImageData model.cropperModel

        ifImage : Html Msg -> Html Msg
        ifImage a =
            if image == Nothing then
                div [] []
            else
                a
    in
        div []
            [ header []
                [ h2 [] [ text "Elm Image Crop Example" ]
                , p []
                    [ text "Here is an image of "
                    , button [ onClick <| CropperMsg <| Cropper.SetImage catImg ] [ text "some cats." ]
                    , text "If you do not like cats then "
                    , button [ onClick <| CropperMsg <| Cropper.SetImage testImg ] [ text "try this instead." ]
                    ]
                , ifImage <| p [] [ text "You can use the sliders below to zoom or position the image. Also try dragging it." ]
                , ifImage <|
                    p []
                        [ text "Here are other sizes to crop to:"
                        , button [ onClick <| CropperMsg <| Cropper.CropTo { width = 240, height = 160 } ] [ text "240×160" ]
                        , button [ onClick <| CropperMsg <| Cropper.CropTo { width = 640, height = 480 } ] [ text "640×480" ]
                        , button [ onClick <| CropperMsg <| Cropper.CropTo { width = 820, height = 312 } ] [ text "820×312" ]
                        , button [ onClick <| CropperMsg <| Cropper.CropTo { width = 1080, height = 608 } ] [ text "1080×608" ]
                        ]
                , ifImage <|
                    p []
                        [ text "Following link sets up the cropper "
                        , a [ href "/?s=/assets/burosch-1920x1080.jpg&w=240&h=130" ] [ text "from javascript" ]
                        , text "."
                        ]
                ]
            , sourceInfoItems model.cropperModel
            , cropper model.cropperModel
            , cropInfoItems model.cropperModel
            , zoomWidget model
            ]


cropper : Cropper.Model -> Html Msg
cropper model =
    case (Cropper.getImageData model) of
        Nothing ->
            div [ class "info-bar" ] [ text "No image loaded..." ]

        Just image ->
            div [] [ Html.map CropperMsg <| Cropper.view model ]


sourceInfoItems : Cropper.Model -> Html Msg
sourceInfoItems model =
    case (Cropper.getImageData model) of
        Nothing ->
            div [ class "info-bar" ] []

        Just image ->
            div [ class "info-bar", style [ ( "max-width", toString image.crop.width ++ "px" ) ] ]
                [ span [] [ "W: " ++ toString image.naturalSize.width |> text ]
                , span [] [ "H: " ++ toString image.naturalSize.height |> text ]
                , span [ class "fill" ] [ "SRC: " ++ image.imageUrl |> text ]
                ]


showRound : Int -> Float -> String
showRound d value =
    let
        f =
            (round (value * toFloat (10 ^ d))) % (10 ^ d)
    in
        toString (floor value) ++ "." ++ (String.padLeft d '0' <| toString f)


cropInfoItems : Cropper.Model -> Html Msg
cropInfoItems model =
    case (Cropper.getImageData model) of
        Nothing ->
            div [ class "info-bar" ] []

        Just image ->
            div [ class "info-bar", style [ ( "max-width", toString image.crop.width ++ "px" ) ] ]
                [ span [] [ "W: " ++ showRound 2 (Cropper.imageSize model).x |> text ]
                , span [] [ "H: " ++ showRound 2 (Cropper.imageSize model).y |> text ]
                , span [] [ "X: " ++ toString (floor (Cropper.cropOrigin model).x) |> text ]
                , span [] [ "Y: " ++ toString (floor (Cropper.cropOrigin model).y) |> text ]
                ]


zoomWidget : Model -> Html Msg
zoomWidget model =
    case (Cropper.getImageData model.cropperModel) of
        Nothing ->
            div [ class "controls" ] []

        Just image ->
            div [ class "controls" ]
                [ p [ class "controls__row" ]
                    [ label [] [ text "Z" ]
                    , input [ onInput Zoom, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (toString image.zoom) ] []
                    , span [] [ text <| showRound 4 image.zoom ]
                    ]
                , p [ class "controls__row" ]
                    [ label [] [ text "X" ]
                    , input [ onInput PivotX, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (toString image.pivot.x) ] []
                    , label [] [ text "Y" ]
                    , input [ onInput PivotY, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (toString image.pivot.y) ] []
                    ]
                , button [ class "controls__button", onClick <| ExportImage ] [ text "Crop" ]
                ]
