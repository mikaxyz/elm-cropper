module Cropper
    exposing
        ( Model
        , Msg
        , view
        , update
        , subscriptions
        , init
        )

{-| Elm Cropper

# Tea
@docs view, update, subscriptions

# Types
@docs Model, Msg

# Helpers
@docs init
-}

import Html exposing (..)
import Html.Attributes exposing (src, class, style)
import Html.Events exposing (on)
import Json.Decode exposing (Decoder)
import DOM
import Util.Debug exposing (..)


type alias Rect =
    { width : Int
    , height : Int
    }


type alias Image =
    { src : String
    , width : Int
    , height : Int
    }


{-| TODO: Doc
-}
type alias Model =
    { url : String
    , crop : Rect
    , image : Maybe Image
    , boundingClientRect : DOM.Rectangle
    }


{-| TODO: Doc
-}
type Msg
    = ImageLoaded Image
    | Measure DOM.Rectangle


{-| TODO: Doc
-}
init : { url : String, crop : { width : Int, height : Int } } -> Model
init { url, crop } =
    { url = url
    , crop = crop
    , image = Nothing
    , boundingClientRect = DOM.Rectangle 0 0 0 0
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
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ImageLoaded image ->
            debugV "ImageLoaded" image ( { model | image = Just image }, Cmd.none )

        Measure rect ->
            debugV "Measure" rect ( { model | boundingClientRect = rect }, Cmd.none )



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
            img [ class "elm-image-cropper__image", src image.src ] []


decodeImage : Json.Decode.Decoder Image
decodeImage =
    Json.Decode.map3 Image
        (Json.Decode.at [ "target", "src" ] Json.Decode.string)
        (Json.Decode.at [ "target", "width" ] Json.Decode.int)
        (Json.Decode.at [ "target", "height" ] Json.Decode.int)


imageOnLoad : Attribute Msg
imageOnLoad =
    on "load" (Json.Decode.map ImageLoaded decodeImage)
