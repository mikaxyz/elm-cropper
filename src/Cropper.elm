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
    }


{-| TODO: Doc
-}
type Msg
    = SetImage { url : String, crop : Rect }


{-| TODO: Doc
-}
init : { url : String, crop : { width : Int, height : Int } } -> Model
init { url, crop } =
    Model url crop Nothing



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
        SetImage { url, crop } ->
            ( model, Cmd.none )



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
        [ div [ class "elm-image-cropper__frame", cropperStyle model.crop ]
            [ img [ class "elm-image-cropper__image", src model.url ] [] ]
        ]
