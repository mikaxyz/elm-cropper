module Cropper.Image exposing (..)

import Util.Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Decode
import Mouse exposing (Position)


-- MODEL


type alias Pivot =
    { x : Float
    , y : Float
    }


type alias Box =
    { width : Int
    , height : Int
    }


type alias Drag =
    { start : Position
    , current : Position
    }


type alias MouseInput =
    { position : Position
    , drag : Maybe Drag
    }


type alias Model =
    { imageUrl : String
    , crop : Box
    , zoom : Float
    , naturalSize : Box
    , pivot : Pivot
    , mouseInput : MouseInput
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
    , mouseInput =
        { position = Position 200 200
        , drag = Nothing
        }
    }



-- UPDATE


type Msg
    = SetImageUrl String
    | SetPivotX Float
    | SetPivotY Float
    | SetZoom Float
    | DragStart Position
    | DragAt Position
    | DragEnd Position


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

        DragStart position ->
            let
                _ =
                    debugOff "DragStart" position
            in
                ( { model | mouseInput = { position = position, drag = Just (Drag position position) } }, Cmd.none )

        DragAt position ->
            let
                _ =
                    debugOn "DragAt" position

                drag : Drag
                drag =
                    case model.mouseInput.drag of
                        Just drag ->
                            drag

                        Nothing ->
                            Drag position position

                dragX =
                    debugOff "dragX" (drag.start.x - position.x)

                dragY =
                    debugOff "dragY" (drag.start.y - position.y)

                pivotX =
                    debugOff "pivotX" (Basics.max 0 (0.5 + (toFloat dragX / toFloat model.crop.width)))

                pivotY =
                    debugOff "pivotY" (Basics.max 0 (0.5 + (toFloat dragY / toFloat model.crop.height)))
            in
                ( { model | pivot = { x = pivotX, y = pivotY }, mouseInput = { position = position, drag = Just drag } }, Cmd.none )

        DragEnd position ->
            let
                _ =
                    debugOff "DragEnd" position
            in
                ( { model | mouseInput = { position = position, drag = Nothing } }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.mouseInput.drag of
        Nothing ->
            debugOff "NoDrag" Sub.none

        Just _ ->
            debugOff "Drag" (Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ])



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
        [ div [ onMouseDown, cropperStyle model.crop, class "cropper__area" ]
            [ img [ imageStyleZoomed model, src model.imageUrl ] []
            ]
        ]


{-| TODO: Understand why I can not use Html.Events.onMouseDown DragStart...
-}
onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Decode.map DragStart Mouse.position)
