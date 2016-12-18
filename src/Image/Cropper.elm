module Image.Cropper exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onWithOptions)
import Json.Decode exposing (Decoder)
import Mouse exposing (Position)
import DOM


{--INTERNAL--}

import Util.Debug exposing (..)
import Image.Types exposing (..)
import Image.Util exposing (..)


-- MODEL


type alias Drag =
    { start : Position
    , current : Position
    }


type alias Model =
    { image : Image
    , position : Position
    , drag : Maybe Drag
    , boundingClientRect : DOM.Rectangle
    }


imageSize : Image -> Vector
imageSize model =
    Image.Util.imageSize model


cropOrigin : Image -> Vector
cropOrigin model =
    Image.Util.cropOrigin model


initialModel : Model
initialModel =
    { image = Image.Util.initialModel
    , position = Position 0 0
    , drag = Nothing
    , boundingClientRect = DOM.Rectangle 0 0 0 0
    }


measureElement : Decoder Msg
measureElement =
    Json.Decode.map Measure (DOM.target <| DOM.boundingClientRect)



-- UPDATE


type Msg
    = SetImage { url : String, width : Int, height : Int }
    | CropTo Box
    | SetPivotX Float
    | SetPivotY Float
    | SetZoom Float
    | DragStart Position
    | DragAt Position
    | DragEnd Position
    | Measure DOM.Rectangle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetImage data ->
            let
                image =
                    model.image
            in
                ( { model
                    | image =
                        { image
                            | imageUrl = data.url
                            , zoom = 0.0
                            , naturalSize =
                                { width = data.width
                                , height = data.height
                                }
                        }
                  }
                , Cmd.none
                )

        CropTo crop ->
            let
                image =
                    model.image
            in
                ( { model | image = { image | crop = crop } }, Cmd.none )

        SetZoom zoom ->
            let
                image =
                    model.image
            in
                ( { model | image = { image | zoom = zoom } }, Cmd.none )

        SetPivotX x ->
            let
                image =
                    model.image

                pivot =
                    model.image.pivot
            in
                ( { model | image = { image | pivot = { pivot | x = x } } }, Cmd.none )

        SetPivotY y ->
            let
                image =
                    model.image

                pivot =
                    model.image.pivot
            in
                ( { model | image = { image | pivot = { pivot | y = y } } }, Cmd.none )

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
                image =
                    model.image

                pivot =
                    getPivot model
            in
                debugOn "DragEnd" ( { model | image = { image | pivot = pivot }, drag = Nothing }, Cmd.none )


dragDistance : Maybe Drag -> Position
dragDistance drag =
    case drag of
        Just drag ->
            Position (drag.start.x - drag.current.x) (drag.start.y - drag.current.y)

        Nothing ->
            Position 0 0


getPivot : Model -> Vector
getPivot model =
    case model.drag of
        Nothing ->
            model.image.pivot

        Just { start, current } ->
            let
                currentHeight =
                    (imageSize model.image).y

                currentWidth =
                    (imageSize model.image).x

                rangeX =
                    debugOff "rangeX" (toFloat model.image.crop.width / (currentWidth - toFloat model.image.crop.width))

                rangeY =
                    debugOff "rangeY" (toFloat model.image.crop.height / (currentHeight - toFloat model.image.crop.height))

                distance =
                    debugOff "dragDistance" (dragDistance model.drag)

                pivotX =
                    (toFloat distance.x / model.boundingClientRect.width) * rangeX

                pivotY =
                    (toFloat distance.y / model.boundingClientRect.height) * rangeY

                dragPivot =
                    debugOff "dragPivot" (Vector pivotX pivotY)
            in
                Vector
                    (Basics.clamp 0.0 1.0 (model.image.pivot.x + dragPivot.x))
                    (Basics.clamp 0.0 1.0 (model.image.pivot.y + dragPivot.y))



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
        -- RELATIVE SIZE
        size =
            imageSize model.image

        width =
            debugOn "width" <| size.x / toFloat model.image.crop.width * 100

        height =
            debugOn "height" <| size.y / toFloat model.image.crop.height * 100

        -- ORIGIN
        currentPivot =
            getPivot model

        posX =
            debug "posX" (-(width - 100.0) * currentPivot.x)

        posY =
            debug "posY" (-(height - 100.0) * currentPivot.y)
    in
        style
            [ ( "position", "absolute" )
            , ( "pointer-events", "none" )
            , ( "width", toString width ++ "%" )
            , ( "height", toString height ++ "%" )
            , ( "left", toString posX ++ "%" )
            , ( "top", toString posY ++ "%" )
            ]


view : Model -> Html Msg
view model =
    div [ class "cropper" ]
        [ div [ class "cropper__area", style [ ( "max-width", toString model.image.crop.width ++ "px" ) ] ]
            [ div [ on "mouseenter" measureElement, onMouseDown, cropperStyle model.image.crop, class "cropper__frame" ] [ img [ imageStyleZoomed model, src model.image.imageUrl ] [] ]
            ]
        ]


onMouseDown : Attribute Msg
onMouseDown =
    onWithOptions "mousedown"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.Decode.map DragStart Mouse.position)
