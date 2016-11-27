module Cropper.Image exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


-- MODEL


type alias Pivot =
    { x : Float
    , y : Float
    }


type alias Box =
    { width : Int
    , height : Int
    }


type alias Model =
    { imageUrl : String
    , crop : Box
    , zoom : Float
    , naturalSize : Box
    , zoomedSize : Float -> Box
    , pivot : Pivot
    }


naturalSize : Box
naturalSize =
    { width = 1024
    , height = 1024
    }


zoomedSize : Float -> Box
zoomedSize zoom =
    { naturalSize
        | width = round (toFloat naturalSize.width * zoom)
        , height = round (toFloat naturalSize.height * zoom)
    }


initialModel : Model
initialModel =
    { imageUrl = "/assets/burosch-1024x1024.jpg"
    , crop =
        { width = 320
        , height = 120
        }
    , zoom = 0.0
    , naturalSize = naturalSize
    , zoomedSize = zoomedSize
    , pivot =
        { x = 0.5
        , y = 0.5
        }
    }



-- UPDATE


type Msg
    = SetImageUrl String
    | SetPivotX Float
    | SetPivotY Float
    | SetZoom Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetImageUrl url ->
            ( { model | imageUrl = url }, Cmd.none )

        SetZoom zoom ->
            ( { model | zoom = zoom }, Cmd.none )

        SetPivotX x ->
            ( { model | pivot = { y = model.pivot.y, x = x } }, Cmd.none )

        SetPivotY y ->
            ( { model | pivot = { y = y, x = model.pivot.x } }, Cmd.none )



-- VIEW


cropperStyle : Box -> Attribute Msg
cropperStyle box =
    style
        [ ( "width", toString box.width ++ "px" )
        , ( "height", toString box.height ++ "px" )
          --        , ( "overflow", "hidden" )
        ]


imageStyle : Box -> Attribute Msg
imageStyle dimensions =
    style
        [ ( "display", "block" )
        , ( "width", toString dimensions.width ++ "%" )
          --        , ( "height", toString dimensions.height ++ "px" )
        ]


imageStyleZoomed : Model -> Attribute Msg
imageStyleZoomed model =
    let
        --        width =
        --            --            Debug.log "width"
        --            (toFloat model.naturalSize.width / toFloat model.crop.width)
        --
        --        height =
        --            --            Debug.log "height"
        --            (toFloat model.naturalSize.height / toFloat model.crop.height)
        --
        --        ratio =
        --            --            Debug.log "ratio"
        --            (1 / Basics.max width height)
        --
        --        max =
        --            --            Debug.log "max"
        --            (toFloat (model.naturalSize.width // model.crop.width))
        --
        --        min =
        --            --            Debug.log "min"
        --            (1 / toFloat (model.naturalSize.width // model.crop.width))
        --
        --        diffY =
        --            Debug.log "diffY"
        --                (toFloat model.crop.height / toFloat model.naturalSize.height)
        --
        --        normalizedZoom =
        --            Debug.log "normalizedZoom"
        --                (1 + (model.zoom * ((toFloat (model.naturalSize.width // model.crop.width)) - 1)))
        normalizedZoomH =
            --            Debug.log "normalizedZoomH"
            (1 + (model.zoom * ((toFloat (model.naturalSize.width // model.crop.width)) - 1)))

        normalizedZoomV =
            --            Debug.log "normalizedZoomV"
            (1 + (model.zoom * ((toFloat (model.naturalSize.height // model.crop.height)) - 1)))

        normalizedZoom =
            --            Debug.log "normalizedZoom"
            (Basics.min normalizedZoomH normalizedZoomV)

        diffY =
            Debug.log "diffY"
                ((toFloat model.naturalSize.height / toFloat model.crop.height) / normalizedZoomH)

        marginX =
            (100.0 * model.pivot.x) - (100.0 * model.pivot.x * normalizedZoomH)

        marginY =
            (100.0 * model.pivot.y) - (100.0 * model.pivot.y * normalizedZoomV)
    in
        style
            [ ( "position", "relative" )
            , ( "opacity", "0.5" )
            , ( "width", toString (100.0 * normalizedZoom) ++ "%" )
            , ( "left", toString marginX ++ "%" )
            , ( "top", toString marginY ++ "%" )
            ]


view : Model -> Html Msg
view model =
    div [ class "cropper" ]
        [ div [ cropperStyle model.crop, class "cropper__area" ]
            [ img [ imageStyleZoomed model, src model.imageUrl ] []
            ]
          --        , p [] [ text <| "ZOOM: " ++ (toString model.zoom) ]
        ]
