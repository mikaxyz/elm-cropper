module Cropper.Image exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


-- MODEL


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
    }


cropBox : Box
cropBox =
    { width = 400
    , height = 300
    }


naturalSize : Box
naturalSize =
    { width = 1600
    , height = 1200
    }


zoomedSize : Float -> Box
zoomedSize zoom =
    { naturalSize
        | width = round (toFloat naturalSize.width * zoom)
        , height = round (toFloat naturalSize.height * zoom)
    }


initialModel : Model
initialModel =
    { imageUrl = "/assets/30192_1600x1200-4-cute-cats.jpg"
    , crop = cropBox
    , zoom = 1.0
    , naturalSize = naturalSize
    , zoomedSize = zoomedSize
    }



-- UPDATE


type Msg
    = SetImageUrl String
    | SetZoom Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetImageUrl url ->
            ( { model | imageUrl = url }, Cmd.none )

        SetZoom zoom ->
            ( { model | zoom = zoom }, Cmd.none )



-- VIEW


cropperStyle : Box -> Attribute Msg
cropperStyle box =
    style
        [ ( "width", toString box.width ++ "px" )
        , ( "height", toString box.height ++ "px" )
        , ( "overflow", "hidden" )
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
        width =
            Debug.log "width"
                (toFloat model.naturalSize.width / toFloat model.crop.width)

        height =
            Debug.log "height"
                (toFloat model.naturalSize.height / toFloat model.crop.height)

        ratio =
            Debug.log "ratio"
                (1 / Basics.max width height)

        max =
            Debug.log "max"
                (toFloat (model.naturalSize.width // model.crop.width))

        min =
            Debug.log "min"
                (1 / toFloat (model.naturalSize.width // model.crop.width))

        zoooom =
            Debug.log "zoooom"
                (toFloat (model.naturalSize.width // model.crop.width) * model.zoom)

        normalizedZoom =
            Debug.log "normalizedZoom"
                (1 + (model.zoom * (max - 1)))
    in
        style
            [ ( "width", toString (100.0 * normalizedZoom) ++ "%" )
              --        , ( "height", toString dimensions.height ++ "px" )
        ]


view : Model -> Html Msg
view model =
    div [ class "cropper" ]
        [ div [ cropperStyle model.crop, class "cropper__area" ]
            [ img [ imageStyleZoomed model, src model.imageUrl ] []
            ]
        , p [] [ text <| "ZOOM: " ++ (toString model.zoom) ]
        ]
