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
        )

{-| Elm Cropper

# Tea
@docs view, update, subscriptions

# Types
@docs Model, Msg

# Helpers
@docs init, zoom, pivotX, pivotY
-}

import Html exposing (..)
import Util.Debug exposing (..)
import Cropper.Types as Types exposing (..)
import Cropper.Helper as Helper exposing (..)
import Cropper.View as View exposing (..)
import DOM


{-| TODO: Doc
-}
type alias Model =
    Types.Model


{-| TODO: Doc
-}
type alias Msg =
    Types.Msg


{-| TODO: Doc
-}
view : Model -> Html Msg
view =
    View.view


{-| TODO: Doc
-}
init : { url : String, crop : { width : Int, height : Int } } -> Model
init { url, crop } =
    { url = url
    , crop = crop
    , image = Nothing
    , boundingClientRect = DOM.Rectangle 0 0 0 0
    , pivot = Vector 0.5 0.5
    , zoom = 0.0
    }


{-| TODO: Doc
-}
zoom : Model -> Float -> Model
zoom =
    Helper.zoom


{-| TODO: Doc
-}
pivotX : Model -> Float -> Model
pivotX =
    Helper.pivotX


{-| TODO: Doc
-}
pivotY : Model -> Float -> Model
pivotY =
    Helper.pivotY



-- SUBSCRIPTIONS


{-| TODO: Doc
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


{-| TODO: Doc
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ImageLoaded image ->
            debugV "ImageLoaded" image ( { model | image = Just image }, Cmd.none )

        Measure rect ->
            debugV "Measure" rect ( { model | boundingClientRect = rect }, Cmd.none )

        Zoom zoom ->
            ( { model | zoom = zoom }, Cmd.none )



-- VIEW
