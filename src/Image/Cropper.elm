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


type alias Model =
    { image : CropperImage
    , position : Position
    , drag : Maybe Drag
    , boundingClientRect : DOM.Rectangle
    }


imageSize : Model -> Vector
imageSize image =
    Image.Util.imageSize <| getImageData image


cropOrigin : Model -> Vector
cropOrigin image =
    Image.Util.cropOrigin <| getImageData image


initialModel : Model
initialModel =
    { image = Loading Image.Util.initialModel
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
    | ImageLoaded String


getImageData : Model -> Image
getImageData model =
    case model.image of
        Unset image ->
            image

        Loading image ->
            image

        Loaded image ->
            image


setImageData : CropperImage -> Image -> CropperImage
setImageData image data =
    case image of
        Unset _ ->
            Unset data

        Loading _ ->
            Loading data

        Loaded _ ->
            Loaded data


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ImageLoaded src ->
            let
                image =
                    debugOn "ImageLoaded" <| getImageData model
            in
                ( { model | image = Loaded image }, Cmd.none )

        SetImage data ->
            let
                current =
                    getImageData model

                image =
                    Loading
                        { current
                            | imageUrl = data.url
                            , zoom = 0.0
                            , naturalSize =
                                { width = data.width
                                , height = data.height
                                }
                        }
            in
                ( { model
                    | image = image
                  }
                , Cmd.none
                )

        CropTo crop ->
            let
                imageData =
                    getImageData model

                image =
                    setImageData model.image { imageData | crop = crop }
            in
                ( { model | image = image }, Cmd.none )

        SetZoom zoom ->
            let
                imageData =
                    getImageData model

                image =
                    setImageData model.image { imageData | zoom = zoom }
            in
                ( { model | image = image }, Cmd.none )

        SetPivotX x ->
            let
                imageData =
                    getImageData model

                pivot =
                    getPivot model

                image =
                    setImageData model.image { imageData | pivot = { pivot | x = x } }
            in
                ( { model | image = image }, Cmd.none )

        SetPivotY y ->
            let
                imageData =
                    getImageData model

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
            let
                imageData =
                    getImageData model

                image =
                    setImageData model.image { imageData | pivot = getPivot model }
            in
                debugOn "DragEnd" ( { model | image = image, drag = Nothing }, Cmd.none )


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
        Unset image ->
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
        Unset image ->
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


targetSrc : String -> Decoder Msg
targetSrc src =
    Json.Decode.succeed (ImageLoaded src)


view : Model -> Html Msg
view model =
    let
        content =
            case model.image of
                Unset image ->
                    [ h4 [ class "elm-image-cropper__loading" ] [ text "Waiting..." ] ]

                Loading image ->
                    [ h4 [ class "elm-image-cropper__loading" ] [ text "Loading..." ]
                    , img [ style [ ( "display", "none" ) ], on "load" <| targetSrc (getImageData model).imageUrl, src (getImageData model).imageUrl ] []
                    ]

                Loaded image ->
                    [ img [ class "elm-image-cropper__image", imageStyle model, src (getImageData model).imageUrl ] [] ]
    in
        div [ class "elm-image-cropper", wrapperStyle (getImageData model).crop ]
            [ div [ class "elm-image-cropper__frame", cropperStyle (getImageData model).crop, on "mouseenter" measureElement, onMouseDown ]
                content
            ]


onMouseDown : Attribute Msg
onMouseDown =
    onWithOptions "mousedown"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.Decode.map DragStart Mouse.position)
