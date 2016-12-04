module Cropper.Image exposing (..)

import Util.Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode exposing (Decoder)
import Mouse exposing (Position)
import Util.DOM as Dom exposing (..)


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


type alias Model =
    { imageUrl : String
    , crop : Box
    , zoom : Float
    , naturalSize : Box
    , pivot : Pivot
    , position : Position
    , drag : Maybe Drag
    , boundingClientRect : Dom.Rectangle
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
    , position = Position 0 0
    , drag = Nothing
    , boundingClientRect = Dom.Rectangle 0 0 0 0
    }


measureElement : Decoder Msg
measureElement =
    Json.Decode.map Measure (Dom.target <| Dom.childNode 0 <| Dom.boundingClientRect)



-- UPDATE


type Msg
    = SetImageUrl String
    | SetPivotX Float
    | SetPivotY Float
    | SetZoom Float
    | DragStart Position
    | DragAt Position
    | DragEnd Position
    | Measure Dom.Rectangle


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

        Measure rect ->
            debugV "Measure" rect ( { model | boundingClientRect = rect }, Cmd.none )

        DragStart xy ->
            debugV "DragStart" xy ( { model | drag = (Just (Drag xy xy)) }, Cmd.none )

        DragAt xy ->
            let
                drag =
                    (Maybe.map (\{ start } -> Drag start xy) model.drag)
            in
                debugOffV "DragAt" model.drag ( { model | drag = drag }, Cmd.none )

        DragEnd _ ->
            let
                pivot =
                    getPivot model
            in
                debugOn "DragEnd" ( { model | pivot = pivot, drag = Nothing }, Cmd.none )


dragDistance : Maybe Drag -> Position
dragDistance drag =
    case drag of
        Just drag ->
            Position (drag.start.x - drag.current.x) (drag.start.y - drag.current.y)

        Nothing ->
            Position 0 0


getPivot : Model -> Pivot
getPivot model =
    case model.drag of
        Nothing ->
            model.pivot

        Just { start, current } ->
            let
                distance =
                    debugOn "dragDistance" (dragDistance model.drag)

                dragPivot =
                    debugOn "dragPivot" (Pivot (toFloat distance.x / model.boundingClientRect.width) (toFloat distance.y / model.boundingClientRect.height))
            in
                Pivot
                    (Basics.clamp 0.0 1.0 (model.pivot.x + dragPivot.x))
                    (Basics.clamp 0.0 1.0 (model.pivot.y + dragPivot.y))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]



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
        currentPivot =
            getPivot model

        posX =
            debug "posX" (-(pWidth - 100.0) * currentPivot.x)

        posY =
            debug "posY" (-(pHeight - 100.0) * currentPivot.y)
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
    div [ on "mouseenter" measureElement, onMouseDown, class "cropper", style [ ( "max-width", toString model.crop.width ++ "px" ) ] ]
        [ div [ cropperStyle model.crop, class "cropper__area" ]
            [ img [ imageStyleZoomed model, src model.imageUrl ] []
            ]
        ]


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Json.Decode.map DragStart Mouse.position)
