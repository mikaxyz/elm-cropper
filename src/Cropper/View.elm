module Cropper.View exposing (boundingClientRect, cropperStyle, decodeBoundingClientRect, imageLoader, imageOnLoad, imageStyle, onDrag, view, wrapperStyle)

import Browser.Dom
import Cropper.Helper as Helper exposing (..)
import Cropper.Types as Types exposing (..)
import DOM
import Html exposing (..)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (on, preventDefaultOn)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Json.Decode exposing (Decoder)


wrapperStyle : Rect -> List (Attribute Msg)
wrapperStyle rect =
    [ ( "-webkit-user-select", "none" )
    , ( "-moz-user-select", "none" )
    , ( "-ms-user-select", "none" )
    , ( "user-select", "none" )
    , ( "max-width", String.fromInt rect.width ++ "px" )
    ]
        |> List.map (\( a, b ) -> style a b)


cropperStyle : Rect -> List (Attribute Msg)
cropperStyle rect =
    [ ( "padding-bottom", String.fromFloat (100.0 * toFloat rect.height / toFloat rect.width) ++ "%" )
    , ( "position", "relative" )
    , ( "height", "0" )
    , ( "overflow", "hidden" )
    , ( "max-width", "100%" )
    ]
        |> List.map (\( a, b ) -> style a b)


imageStyle : Model -> List (Attribute Msg)
imageStyle model =
    case model.image of
        Nothing ->
            []

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
            [ ( "position", "absolute" )
            , ( "min-width", "0" )
            , ( "max-width", "none" )
            , ( "display", "block" )
            , ( "pointer-events", "none" )
            , ( "width", String.fromFloat width ++ "%" )
            , ( "height", String.fromFloat height ++ "%" )
            , ( "left", String.fromFloat posX ++ "%" )
            , ( "top", String.fromFloat posY ++ "%" )
            ]
                |> List.map (\( a, b ) -> style a b)


{-| TODO: Doc
-}
view : Model -> Html Msg
view model =
    div (List.append [ class "elm-image-cropper" ] (wrapperStyle model.crop))
        [ div (List.append (cropperStyle model.crop) ([ class "elm-image-cropper__frame" ] ++ onDrag))
            [ div (cropperStyle model.crop ++ boundingClientRect)
                [ imageLoader model ]
            ]
        ]


onDrag : List (Attribute Msg)
onDrag =
    [ Mouse.onDown
        (\event ->
            DragStart
                { x = event.offsetPos |> Tuple.first |> round
                , y = event.offsetPos |> Tuple.second |> round
                }
        )
    , Touch.onStart (OnTouchStart << touchCoordinates)
    , Touch.onMove (OnTouchMove << touchCoordinates)
    , Touch.onEnd (OnTouchEnd << touchCoordinates)
    ]


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .pagePos
        |> Maybe.withDefault ( 0, 0 )


boundingClientRect : List (Attribute Msg)
boundingClientRect =
    [ preventDefaultOn "mousedown" (Json.Decode.map alwaysPreventDefault decodeBoundingClientRect)
    , preventDefaultOn "touchstart" (Json.Decode.map alwaysPreventDefault decodeBoundingClientRect)
    ]


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


decodeBoundingClientRect : Decoder Msg
decodeBoundingClientRect =
    Json.Decode.map Measure (DOM.target <| DOM.boundingClientRect)


imageLoader : Model -> Html Msg
imageLoader model =
    case model.image of
        Nothing ->
            div []
                [ h4 [ class "elm-image-cropper__loading" ] [ text "Loading..." ]
                , img [ style "display" "none", imageOnLoad, src model.url ] []
                ]

        Just image ->
            img (List.append (imageStyle model) [ class "elm-image-cropper__image", src image.src ]) []


imageOnLoad : Attribute Msg
imageOnLoad =
    on "load" (Json.Decode.map ImageLoaded decodeImage)
