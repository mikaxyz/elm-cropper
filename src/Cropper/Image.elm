module Cropper.Image exposing (..)

import Util.Debug exposing (..)
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
    , pivot : Pivot
    }


initialModel : Model
initialModel =
    { imageUrl = "/assets/30192_1600x1200-4-cute-cats.jpg"
    , crop =
        { width = 820
        , height = 312
        }
    , zoom = 0.5
    , naturalSize =
        { width = 1600
        , height = 1200
        }
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


cropperStyle : Box -> Attribute Msg
cropperStyle box =
    style
        [ ( "padding-bottom", toString (100.0 * toFloat box.height / toFloat box.width) ++ "%" )
        ]


imageStyleZoomed : Model -> Attribute Msg
imageStyleZoomed model =
    let
        ratioH =
            debug "ratioH" (toFloat model.naturalSize.width / toFloat model.crop.width)

        ratioV =
            debug "ratioV" (toFloat model.naturalSize.height / toFloat model.crop.height)

        ratioMin =
            debug "ratioMin" (Basics.min ratioH ratioV)

        -- ZOOM
        pWidth =
            debug "pWidth" ((100.0 * ratioH / ratioMin) * (1 + model.zoom))

        pHeight =
            debug "pHeight" ((100.0 * ratioV / ratioMin) * (1 + model.zoom))

        -- ORIGIN
        posX =
            debug "posX" (-(pWidth - 100.0) * model.pivot.x)

        posY =
            debug "posY" (-(pHeight - 100.0) * model.pivot.y)
    in
        style
            [ ( "position", "absolute" )
            , ( "pointer-events", "none" )
            , ( "width", toString pWidth ++ "%" )
            , ( "height", toString pHeight ++ "%" )
            , ( "left", toString posX ++ "%" )
            , ( "top", toString posY ++ "%" )
            ]


view : Model -> Html Msg
view model =
    div [ class "cropper", style [ ( "max-width", toString model.crop.width ++ "px" ) ] ]
        [ div [ cropperStyle model.crop, class "cropper__area" ]
            [ img [ imageStyleZoomed model, src model.imageUrl ] []
            ]
        ]
