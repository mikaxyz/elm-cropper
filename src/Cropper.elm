module Cropper
    exposing
        ( Model
        , Msg
        , view
        , update
        , subscriptions
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
import Html.Attributes exposing (src, class, style)
import Html.Events exposing (on)
import Json.Decode exposing (Decoder)
import DOM
import Util.Debug exposing (..)
import Cropper.Types as Types exposing (..)
import Cropper.Helper as Helper exposing (..)


{-| TODO: Doc
-}
type alias Model =
    { url : String
    , crop : Rect
    , image : Maybe Image
    , boundingClientRect : DOM.Rectangle
    , pivot : Vector
    , zoom : Float
    }


{-| TODO: Doc
-}
type Msg
    = ImageLoaded Image
    | Measure DOM.Rectangle
    | Zoom Float


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



-- SUBSCRIPTIONS


{-| TODO: Doc
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


{-| TODO: Doc
-}
zoom : Model -> Float -> Model
zoom model zoom =
    { model | zoom = zoom }


{-| TODO: Doc
-}
pivotX : Model -> Float -> Model
pivotX model x =
    let
        pivot =
            model.pivot
    in
        { model | pivot = { pivot | x = Basics.clamp 0.0 1.0 x } }


{-| TODO: Doc
-}
pivotY : Model -> Float -> Model
pivotY model y =
    let
        pivot =
            model.pivot
    in
        { model | pivot = { pivot | y = Basics.clamp 0.0 1.0 y } }


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


wrapperStyle : Rect -> Attribute Msg
wrapperStyle rect =
    style
        [ ( "-webkit-user-select", "none" )
        , ( "-moz-user-select", "none" )
        , ( "-ms-user-select", "none" )
        , ( "user-select", "none" )
        , ( "max-width", toString rect.width ++ "px" )
        ]


cropperStyle : Rect -> Attribute Msg
cropperStyle rect =
    style
        [ ( "padding-bottom", toString (100.0 * toFloat rect.height / toFloat rect.width) ++ "%" )
        , ( "position", "relative" )
        , ( "height", "0" )
        , ( "overflow", "hidden" )
        , ( "max-width", "100%" )
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
        [ div [ class "elm-image-cropper__frame", boundingClientRect, cropperStyle model.crop ]
            [ imageLoader model ]
        ]


boundingClientRect : Attribute Msg
boundingClientRect =
    on "mouseenter" decodeBoundingClientRect


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


decodeImage : Json.Decode.Decoder Image
decodeImage =
    Json.Decode.map3 Image
        (Json.Decode.at [ "target", "src" ] Json.Decode.string)
        (Json.Decode.at [ "target", "width" ] Json.Decode.int)
        (Json.Decode.at [ "target", "height" ] Json.Decode.int)


imageOnLoad : Attribute Msg
imageOnLoad =
    on "load" (Json.Decode.map ImageLoaded decodeImage)


{-|


asd
as
das
d
asd

-}
getPivot : Model -> Vector
getPivot model =
    model.pivot
