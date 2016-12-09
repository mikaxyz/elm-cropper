module Main exposing (..)

import Util.Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onMouseDown, onMouseUp)
import Cropper.Image as Cropper
import Cropper.Mouse as Mouse
import Util.DOM as Dom exposing (..)
import Json.Decode exposing (Decoder)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { name : String
    , cropperModel : Cropper.Model
    , mouseModel : Mouse.Model
    , boundingClientRect : Dom.Rectangle
    }


initialModel : Model
initialModel =
    { name = "I am sand. Sand box."
    , cropperModel = Cropper.initialModel
    , mouseModel = Mouse.initialModel
    , boundingClientRect = Dom.Rectangle 0 0 0 0
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = Set String
    | Zoom String
    | PivotX String
    | PivotY String
    | CropperMsg Cropper.Msg
    | MouseMsg Mouse.Msg
    | Measure Dom.Rectangle


measureWidth : Dom.Rectangle -> Float
measureWidth rect =
    rect.width - rect.left


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Measure rect ->
            let
                _ =
                    debugOn "Measure" rect
            in
                ( { model | boundingClientRect = rect }, Cmd.none )

        Set to ->
            ( model, Cmd.none )

        Zoom zoom ->
            update (CropperMsg <| Cropper.SetZoom (Result.withDefault 0 (String.toFloat zoom))) model

        PivotX x ->
            update (CropperMsg <| Cropper.SetPivotX (Result.withDefault 0 (String.toFloat x))) model

        PivotY y ->
            update (CropperMsg <| Cropper.SetPivotY (Result.withDefault 0 (String.toFloat y))) model

        CropperMsg subMsg ->
            let
                _ =
                    debugOn "CropperMsg" subMsg

                ( updatedSubModel, subCmd ) =
                    Cropper.update subMsg model.cropperModel
            in
                ( { model | cropperModel = updatedSubModel }, Cmd.map CropperMsg subCmd )

        MouseMsg subMsg ->
            let
                ( updatedSubModel, subCmd ) =
                    Mouse.update subMsg model.mouseModel

                _ =
                    debugOn "MouseMsg" ( subMsg, updatedSubModel )

                drag : Mouse.Drag
                drag =
                    case updatedSubModel.drag of
                        Just drag ->
                            drag

                        Nothing ->
                            Mouse.Drag model.mouseModel.position model.mouseModel.position model.mouseModel.position

                cropperModel =
                    model.cropperModel

                dragX =
                    debugOff "dragX" (drag.start.x - updatedSubModel.position.x)

                dragY =
                    debugOff "dragY" (drag.start.y - updatedSubModel.position.y)

                pivotX =
                    debugOff "pivotX" (Basics.max 0 (0.5 + (toFloat dragX / toFloat cropperModel.crop.width)))

                pivotY =
                    debugOff "pivotY" (Basics.max 0 (0.5 + (toFloat dragY / toFloat cropperModel.crop.height)))
            in
                ( { model | mouseModel = updatedSubModel, cropperModel = { cropperModel | pivot = { x = pivotX, y = pivotY } } }, Cmd.map MouseMsg subCmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map CropperMsg (Cropper.subscriptions model.cropperModel)
        , Sub.map MouseMsg (Mouse.subscriptions model.mouseModel)
        ]



-- VIEW


measureElement : Decoder Msg
measureElement =
    Json.Decode.map Measure (Dom.target <| Dom.childNode 0 <| Dom.boundingClientRect)


view : Model -> Html Msg
view model =
    div []
        [ h4 [] [ text model.name ]
        , div [ on "mouseenter" measureElement, onMouseDown (MouseMsg Mouse.StartDrag), onMouseUp (MouseMsg Mouse.StopDrag), class "section" ] [ Html.map CropperMsg <| Cropper.view model.cropperModel ]
          --        , a [ class "button", onClick <| CropperMsg <| Cropper.SetImageUrl "https://i.ytimg.com/vi/opKg3fyqWt4/hqdefault.jpg" ] [ text "Make pup" ]
        , zoomWidget model
        ]


zoomWidget : Model -> Html Msg
zoomWidget model =
    div [ class "cropper__controls" ]
        [ h4 [] [ text <| "ZOOM: " ++ (toString model.cropperModel.zoom) ]
        , input [ style [ ( "width", "50%" ) ], onInput Zoom, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (toString model.cropperModel.zoom) ] []
        , input [ style [ ( "width", "50%" ) ], onInput PivotX, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (toString model.cropperModel.pivot.x) ] [ text "asdas" ]
        , input [ style [ ( "width", "50%" ) ], onInput PivotY, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (toString model.cropperModel.pivot.y) ] []
        ]
