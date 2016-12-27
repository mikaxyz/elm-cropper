module Image.Cropper exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onWithOptions)
import Json.Decode exposing (Decoder)
import Mouse exposing (Position)
import DOM
import Util.Debug exposing (..)
import Image.Types exposing (..)
import Image.Util exposing (..)


-- MODEL


type alias Drag =
    { start : Position
    , current : Position
    }


type alias ImageData =
    { src : String
    , width : Int
    , height : Int
    }


type alias Model =
    { image : CropperImage
    , position : Position
    , drag : Maybe Drag
    , boundingClientRect : DOM.Rectangle
    }


imageSize : Model -> Vector
imageSize image =
    case (getImageData image) of
        Nothing ->
            Vector 0 0

        Just imageData ->
            Image.Util.imageSize <| imageData


cropOrigin : Model -> Vector
cropOrigin image =
    case (getImageData image) of
        Nothing ->
            Vector 0 0

        Just imageData ->
            Image.Util.cropOrigin <| imageData


initialModel : Model
initialModel =
    { image = Unset
    , position = Position 0 0
    , drag = Nothing
    , boundingClientRect = DOM.Rectangle 0 0 0 0
    }


measureElement : Decoder Msg
measureElement =
    Json.Decode.map Measure (DOM.target <| DOM.boundingClientRect)



-- UPDATE


type Msg
    = SetImage { url : String, crop : Box }
    | CropTo Box
    | SetPivotX Float
    | SetPivotY Float
    | SetZoom Float
    | DragStart Position
    | DragAt Position
    | DragEnd Position
    | Measure DOM.Rectangle
    | ImageLoaded ImageData


getImageData : Model -> Maybe Image
getImageData model =
    case model.image of
        Unset ->
            Nothing

        Loading image ->
            Just image

        Loaded image ->
            Just image


setImageData : CropperImage -> Image -> CropperImage
setImageData image data =
    case image of
        Unset ->
            Unset

        Loading _ ->
            Loading data

        Loaded _ ->
            Loaded data


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ImageLoaded data ->
            let
                _ =
                    debugOn "ImageLoaded" data

                naturalSize =
                    { width = data.width
                    , height = data.height
                    }
            in
                case (getImageData model) of
                    Nothing ->
                        ( model, Cmd.none )

                    Just imageData ->
                        ( { model | image = Loaded { imageData | naturalSize = naturalSize } }, Cmd.none )

        SetImage data ->
            let
                image =
                    Loading
                        { imageUrl = data.url
                        , crop =
                            { width = data.crop.width
                            , height = data.crop.height
                            }
                        , zoom = 0.0
                        , naturalSize =
                            { width = 0
                            , height = 0
                            }
                        , pivot =
                            { x = 0.5
                            , y = 0.5
                            }
                        }
            in
                ( { model | image = image }, Cmd.none )

        CropTo crop ->
            case (getImageData model) of
                Nothing ->
                    ( model, Cmd.none )

                Just imageData ->
                    ( { model | image = setImageData model.image { imageData | crop = crop } }, Cmd.none )

        SetZoom zoom ->
            case (getImageData model) of
                Nothing ->
                    ( model, Cmd.none )

                Just imageData ->
                    ( { model | image = setImageData model.image { imageData | zoom = zoom } }, Cmd.none )

        SetPivotX x ->
            case (getImageData model) of
                Nothing ->
                    ( model, Cmd.none )

                Just imageData ->
                    let
                        pivot =
                            getPivot model

                        image =
                            setImageData model.image { imageData | pivot = { pivot | x = x } }
                    in
                        ( { model | image = image }, Cmd.none )

        SetPivotY y ->
            case (getImageData model) of
                Nothing ->
                    ( model, Cmd.none )

                Just imageData ->
                    let
                        pivot =
                            getPivot model

                        image =
                            setImageData model.image { imageData | pivot = { pivot | y = y } }
                    in
                        ( { model | image = image }, Cmd.none )

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
            case (getImageData model) of
                Nothing ->
                    ( model, Cmd.none )

                Just imageData ->
                    debugOn "DragEnd" ( { model | image = setImageData model.image { imageData | pivot = getPivot model }, drag = Nothing }, Cmd.none )


dragDistance : Maybe Drag -> Position
dragDistance drag =
    case drag of
        Just drag ->
            Position (drag.start.x - drag.current.x) (drag.start.y - drag.current.y)

        Nothing ->
            Position 0 0


getPivot : Model -> Vector
getPivot model =
    case model.image of
        Unset ->
            Vector 0.5 0.5

        Loading image ->
            Vector 0.5 0.5

        Loaded image ->
            case model.drag of
                Nothing ->
                    image.pivot

                Just { start, current } ->
                    let
                        currentHeight =
                            (imageSize model).y

                        currentWidth =
                            (imageSize model).x

                        rangeX =
                            debugOff "rangeX" (toFloat image.crop.width / (currentWidth - toFloat image.crop.width))

                        rangeY =
                            debugOff "rangeY" (toFloat image.crop.height / (currentHeight - toFloat image.crop.height))

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
                            (Basics.clamp 0.0 1.0 (image.pivot.x + dragPivot.x))
                            (Basics.clamp 0.0 1.0 (image.pivot.y + dragPivot.y))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]



-- VIEW


wrapperStyle : Box -> Attribute Msg
wrapperStyle box =
    style
        [ ( "-webkit-user-select", "none" )
        , ( "-moz-user-select", "none" )
        , ( "-ms-user-select", "none" )
        , ( "user-select", "none" )
        , ( "max-width", toString box.width ++ "px" )
        ]


cropperStyle : Box -> Attribute Msg
cropperStyle box =
    style
        [ ( "padding-bottom", toString (100.0 * toFloat box.height / toFloat box.width) ++ "%" )
        , ( "position", "relative" )
        , ( "height", "0" )
        , ( "overflow", "hidden" )
        , ( "max-width", "100%" )
        ]


imageStyle : Model -> Attribute Msg
imageStyle model =
    case model.image of
        Unset ->
            style [ ( "background-color", "red" ) ]

        Loading image ->
            style [ ( "background-color", "green" ) ]

        Loaded image ->
            let
                -- RELATIVE SIZE
                size =
                    imageSize model

                width =
                    debugOn "width" <| size.x / toFloat image.crop.width * 100

                height =
                    debugOn "height" <| size.y / toFloat image.crop.height * 100

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
                    , ( "min-width", "0" )
                    , ( "max-width", "none" )
                    , ( "display", "block" )
                    , ( "pointer-events", "none" )
                    , ( "width", toString width ++ "%" )
                    , ( "height", toString height ++ "%" )
                    , ( "left", toString posX ++ "%" )
                    , ( "top", toString posY ++ "%" )
                    ]


imageOnLoad : Attribute Msg
imageOnLoad =
    on "load" (Json.Decode.map ImageLoaded imageData)


imageData : Json.Decode.Decoder ImageData
imageData =
    Json.Decode.map3 ImageData
        (Json.Decode.at [ "target", "src" ] Json.Decode.string)
        (Json.Decode.at [ "target", "width" ] Json.Decode.int)
        (Json.Decode.at [ "target", "height" ] Json.Decode.int)


view : Model -> Html Msg
view model =
    let
        content =
            case model.image of
                Unset ->
                    [ h4 [ class "elm-image-cropper__loading" ] [ text "Waiting..." ] ]

                Loading image ->
                    [ h4 [ class "elm-image-cropper__loading" ] [ text "Loading..." ]
                    , img [ style [ ( "display", "none" ) ], imageOnLoad, src image.imageUrl ] []
                    ]

                Loaded image ->
                    [ img [ class "elm-image-cropper__image", imageStyle model, src image.imageUrl ] [] ]

        wrapperStyleC =
            case getImageData model of
                Nothing ->
                    style []

                Just imageData ->
                    wrapperStyle imageData.crop

        cropperStyleC =
            case getImageData model of
                Nothing ->
                    style []

                Just imageData ->
                    cropperStyle imageData.crop
    in
        div [ class "elm-image-cropper", wrapperStyleC ]
            [ div [ class "elm-image-cropper__frame", cropperStyleC, on "mouseenter" measureElement, onMouseDown ]
                content
            ]


onMouseDown : Attribute Msg
onMouseDown =
    onWithOptions "mousedown"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.Decode.map DragStart Mouse.position)
