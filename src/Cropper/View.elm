module Cropper.View exposing (..)

import Cropper.Types as Types exposing (..)
import Cropper.Helper as Helper exposing (..)
import Html exposing (..)
import Html.Attributes exposing (src, class, style)
import Html.Events exposing (on, onWithOptions)
import Json.Decode exposing (Decoder)
import DOM
import Mouse exposing (Position)
import Touch exposing (TouchEvent(..), Touch)
import SingleTouch exposing (onSingleTouch)


wrapperStyle : Rect -> Attribute Msg
wrapperStyle rect =
    style
        [ ( "-webkit-user-select", "none" )
        , ( "-moz-user-select", "none" )
        , ( "-ms-user-select", "none" )
        , ( "user-select", "none" )
        , ( "max-width", toString rect.width ++ "px" )
        , ( "position", "relative" )
        ]


cropperBackdropStyle : Rect -> Attribute Msg
cropperBackdropStyle rect =
    style
        [ ( "padding-bottom", toString (100.0 * toFloat rect.height / toFloat rect.width) ++ "%" )
        , ( "position", "relative" )
        , ( "height", "0" )

        --        , ( "overflow", "hidden" )
        , ( "opacity", "0.3" )
        , ( "max-width", "100%" )
        ]


cropperStyle : Rect -> Attribute Msg
cropperStyle rect =
    style
        [ ( "padding-bottom", toString (100.0 * toFloat rect.height / toFloat rect.width) ++ "%" )
        , ( "position", "absolute" )
        , ( "height", "0" )
        , ( "overflow", "hidden" )
        , ( "max-width", "100%" )
        , ( "width", "100%" )
        , ( "top", "0" )
        ]


imageStyle : Model -> Attribute Msg
imageStyle model =
    case model.image of
        Nothing ->
            style []

        Just image ->
            let
                -- RELATIVE SIZE
                size =
                    imageSize
                        { image = image
                        , crop = model.crop
                        , zoom = model.zoom
                        }

                width =
                    size.x / toFloat model.crop.width * 100

                height =
                    size.y / toFloat model.crop.height * 100

                -- ORIGIN
                currentPivot =
                    getPivot model

                posX =
                    -(width - 100.0) * currentPivot.x

                posY =
                    -(height - 100.0) * currentPivot.y
            in
                style
                    [ ( "position", "absolute" )
                    , ( "min-width", "0" )
                    , ( "max-width", "none" )
                    , ( "display", "block" )
                    , ( "pointer-events", "none" )
                    , ( "width", toString width ++ "%" )
                    , ( "height", toString height ++ "%" )
                    , ( "left", toString posX ++ "%" )
                    , ( "top", toString posY ++ "%" )
                    ]


{-| TODO: Doc
-}
view : Model -> Html Msg
view model =
    div [ class "elm-image-cropper", wrapperStyle model.crop ]
        [ div ([ class "elm-image-cropper__frame", cropperBackdropStyle model.crop ] ++ onDrag)
            [ div ([ cropperBackdropStyle model.crop ] ++ boundingClientRect)
                [ imageLoader model ]
            ]
        , div ([ class "elm-image-cropper__frame", cropperStyle model.crop ] ++ onDrag)
            [ div ([ cropperStyle model.crop ] ++ boundingClientRect)
                [ imageLoader model ]
            ]
        ]


onDrag : List (Attribute Msg)
onDrag =
    [ onWithOptions "mousedown" Touch.preventAndStop <| (Json.Decode.map DragStart Mouse.position)
    , onSingleTouch TouchStart Touch.preventAndStop <| OnTouchStart
    , onSingleTouch TouchMove Touch.preventAndStop <| OnTouchMove
    , onSingleTouch TouchEnd Touch.preventAndStop <| OnTouchEnd
    , onSingleTouch TouchCancel Touch.preventAndStop <| OnTouchEnd
    ]


boundingClientRect : List (Attribute Msg)
boundingClientRect =
    let
        options =
            { stopPropagation = False
            , preventDefault = True
            }
    in
        [ onWithOptions "mousedown" options decodeBoundingClientRect
        , onWithOptions "touchstart" options decodeBoundingClientRect
        ]


decodeBoundingClientRect : Decoder Msg
decodeBoundingClientRect =
    Json.Decode.map Measure (DOM.target <| DOM.boundingClientRect)


imageLoader : Model -> Html Msg
imageLoader model =
    case model.image of
        Nothing ->
            div []
                [ h4 [ class "elm-image-cropper__loading" ] [ text "Loading..." ]
                , img [ style [ ( "display", "none" ) ], imageOnLoad, src model.url ] []
                ]

        Just image ->
            img [ class "elm-image-cropper__image", imageStyle model, src image.src ] []


imageOnLoad : Attribute Msg
imageOnLoad =
    on "load" (Json.Decode.map ImageLoaded decodeImage)
