module Cropper
    exposing
        ( view
        , update
        , subscriptions
        , Model
        , Msg
        , init
        , zoom
        , pivotX
        , pivotY
        , crop
        , cropData
        , imageSize
        , cropOrigin
        )

{-| Fluid width/responsive image cropper UI

# TEA
@docs init, view, update, subscriptions

## Types
@docs Model, Msg

# Helpers

## Getters
@docs cropData, imageSize, cropOrigin

## Setters
@docs zoom, pivotX, pivotY, crop
-}

import Html exposing (..)
import Cropper.Types as Types exposing (..)
import Cropper.Helper as Helper exposing (..)
import Cropper.View as View exposing (..)
import DOM
import Mouse exposing (Position)


{-| State of the cropper
-}
type alias Model =
    Types.Model


{-| Messages
-}
type alias Msg =
    Types.Msg


{-| TEA View
-}
view : Model -> Html Msg
view =
    View.view


{-| Use this function to initialize the module with url to image and a crop size.
-}
init : { url : String, crop : { width : Int, height : Int } } -> Model
init { url, crop } =
    { url = url
    , crop = crop
    , image = Nothing
    , boundingClientRect = DOM.Rectangle 0 0 0 0
    , pivot = Vector 0.5 0.5
    , zoom = 0.0
    , drag = Nothing
    }


{-| Set zoom (clamped to 0.0...1.0)
-}
zoom : Model -> Float -> Model
zoom =
    Helper.zoom


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
    Helper.crop


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
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]



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

        Zoom zoom ->
            ( { model | zoom = zoom }, Cmd.none )

        DragStart xy ->
            ( { model | drag = (Just (Drag xy xy)) }, Cmd.none )

        DragEnd xy ->
            ( { model | pivot = getPivot model, drag = Nothing }, Cmd.none )

        DragAt xy ->
            let
                drag =
                    (Maybe.map (\{ start } -> Drag start xy) model.drag)
            in
                ( { model | drag = drag }, Cmd.none )

        OnTouchStart touch ->
            let
                xy =
                    Position (round touch.touch.clientX) (round touch.touch.clientY)
            in
                ( { model | drag = (Just (Drag xy xy)) }, Cmd.none )

        OnTouchMove touch ->
            let
                xy =
                    Position (round touch.touch.clientX) (round touch.touch.clientY)

                drag =
                    (Maybe.map (\{ start } -> Drag start xy) model.drag)
            in
                ( { model | drag = drag }, Cmd.none )

        OnTouchEnd xy ->
            ( { model | pivot = getPivot model, drag = Nothing }, Cmd.none )
