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
    { width = 460
    , height = 320
    }


naturalSize : Box
naturalSize =
    { width = 800
    , height = 300
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
        [ ( "width", toString dimensions.width ++ "px" )
        , ( "height", toString dimensions.height ++ "px" )
        ]


view : Model -> Html Msg
view model =
    div [ class "cropper" ]
        [ div [ cropperStyle model.crop, class "cropper__area" ]
            [ img [ imageStyle <| model.zoomedSize model.zoom, src model.imageUrl ] []
            ]
        , p [] [ text <| "ZOOM: " ++ (toString model.zoom) ]
        ]
