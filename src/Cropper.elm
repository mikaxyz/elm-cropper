module Cropper exposing
    ( init, view, update, subscriptions
    , Model, Msg, ImageData, CropData
    , cropData, imageSize, cropOrigin
    , zoom, pivotX, pivotY, crop
    )

{-| Fluid width/responsive image cropper UI


# TEA

@docs init, view, update, subscriptions


## Types

@docs Model, Msg, ImageData, CropData


# Helpers


## Getters

@docs cropData, imageSize, cropOrigin


## Setters

@docs zoom, pivotX, pivotY, crop

-}

import Browser.Dom
import Browser.Events
import Cropper.Helper as Helper exposing (..)
import Cropper.Types as Types exposing (..)
import Cropper.View as View exposing (..)
import DOM
import Html exposing (..)
import Json.Decode



--import Mouse exposing (Position)


{-| State of the cropper
-}
type alias Model =
    Types.Model


{-| Messages
-}
type alias Msg =
    Types.Msg


{-| Helper type intended for wrapping data from javascript via port
Example:
port cropperWithImage : (ImageData -> msg) -> Sub msg
-}
type alias ImageData =
    Types.ImageData


{-| Helper type intended for wrapping data sent to javascript via port
Example:
port cropperData : CropData -> Cmd msg
-}
type alias CropData =
    Types.CropData


{-| TEA View
-}
view : Model -> Html Msg
view =
    View.view


{-| Use this function to initialize the module with url to image and a crop size.
-}
init : { url : String, crop : { width : Int, height : Int } } -> Model
init a_ =
    { url = a_.url
    , crop = a_.crop
    , image = Nothing
    , boundingClientRect = DOM.Rectangle 0 0 0 0
    , pivot = Vector 0.5 0.5
    , zoom = 1.0
    , drag = Nothing
    }


{-| Set zoom (clamped to 0.0...1.0)
-}
zoom : Model -> Float -> Model
zoom =
    Helper.zoomImage


{-| Set horizontal pivot (clamped to 0.0...1.0)
-}
pivotX : Model -> Float -> Model
pivotX =
    Helper.pivotX


{-| Set vertical pivot (clamped to 0.0...1.0)
-}
pivotY : Model -> Float -> Model
pivotY =
    Helper.pivotY


{-| Set crop size. Will be limited to image size.
-}
crop : Model -> { width : Int, height : Int } -> Model
crop =
    Helper.cropImage


{-| Get image size
-}
imageSize : Model -> Vector
imageSize model =
    case model.image of
        Nothing ->
            Vector 0 0

        Just image ->
            Helper.imageSize { image = image, crop = model.crop, zoom = model.zoom }


{-| Get starting point of crop area over imageSize
-}
cropOrigin : Model -> Vector
cropOrigin model =
    case model.image of
        Nothing ->
            Vector 0 0

        Just image ->
            Helper.cropOrigin { image = image, crop = model.crop, pivot = model.pivot, zoom = model.zoom }


{-| Get all data required (by CanvasRenderingContext2D.drawImage) to crop the image
-}
cropData : Model -> CropData
cropData =
    Helper.cropData



-- SUBSCRIPTIONS


{-| TEA subscriptions needs to be hooked up for mouse dragging
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch
                [ Browser.Events.onMouseMove (Json.Decode.map2 (fromPoint DragAt) pageX pageY)
                , Browser.Events.onMouseUp (Json.Decode.map2 (fromPoint DragEnd) pageX pageY)
                ]


fromPoint : (Point -> Msg) -> Int -> Int -> Msg
fromPoint msg x y =
    Point x y |> msg


pageX : Json.Decode.Decoder Int
pageX =
    Json.Decode.field "pageX" Json.Decode.int


pageY : Json.Decode.Decoder Int
pageY =
    Json.Decode.field "pageY" Json.Decode.int



--decodeMouse : Json.Decode.Decoder Position
--decodeMouse =
--    Json.Decode.map2 Position
--        (Json.Decode.at [ "offsetX" ] Json.Decode.int)
--        (Json.Decode.at [ "offsetY" ] Json.Decode.int)
-- UPDATE


{-| TEA update function
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ImageLoaded image ->
            ( crop { model | image = Just image } model.crop, Cmd.none )

        Measure rect ->
            ( { model | boundingClientRect = rect }, Cmd.none )

        Zoom zoom_ ->
            ( { model | zoom = zoom_ }, Cmd.none )

        DragStart pos ->
            ( { model | drag = Just (Drag pos pos) }, Cmd.none )

        DragEnd _ ->
            ( { model | pivot = getPivot model, drag = Nothing }, Cmd.none )

        DragAt pagePos ->
            let
                pos =
                    { x = pagePos.x - round model.boundingClientRect.left
                    , y = pagePos.y - round model.boundingClientRect.top
                    }

                drag =
                    Maybe.map (\{ start } -> Drag start pos) model.drag
            in
            ( { model | drag = drag }, Cmd.none )

        OnTouchStart ( x, y ) ->
            let
                pos =
                    Position (round x) (round y)
            in
            ( { model | drag = Just (Drag pos pos) }, Cmd.none )

        OnTouchMove ( x, y ) ->
            let
                xy =
                    Position (round x) (round y)

                drag =
                    Maybe.map (\{ start } -> Drag start xy) model.drag
            in
            ( { model | drag = drag }, Cmd.none )

        OnTouchEnd xy ->
            ( { model | pivot = getPivot model, drag = Nothing }, Cmd.none )
