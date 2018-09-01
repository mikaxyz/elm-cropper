port module Main exposing (Model, Msg(..), cropInfoItems, cropperData, cropperWithImage, init, main, showRound, sourceInfoItems, subscriptions, update, view)

import Browser exposing (element)
import Cropper
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


port cropperWithImage : (Cropper.ImageData -> msg) -> Sub msg


port cropperData : Cropper.CropData -> Cmd msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cropper =
            Cropper.init
                { url = "assets/kittens-1280x711.jpg"
                , crop = { width = 720, height = 480 }
                }
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ToCropper (Cropper.subscriptions model.cropper)
        , cropperWithImage CropImage
        ]


type Msg
    = ToCropper Cropper.Msg
    | Zoom String
    | PivotX String
    | PivotY String
    | CropImage { url : String, crop : { width : Int, height : Int } }
    | Crop { width : Int, height : Int }
    | ExportImage



-- MODEL


type alias Model =
    { cropper : Cropper.Model
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToCropper subMsg ->
            let
                ( updatedSubModel, subCmd ) =
                    Cropper.update subMsg model.cropper
            in
            ( { model | cropper = updatedSubModel }, Cmd.map ToCropper subCmd )

        Zoom zoom ->
            ( { model | cropper = Cropper.zoom model.cropper (Maybe.withDefault 0 (String.toFloat zoom)) }, Cmd.none )

        PivotX x ->
            ( { model | cropper = Cropper.pivotX model.cropper (Maybe.withDefault 0 (String.toFloat x)) }, Cmd.none )

        PivotY y ->
            ( { model | cropper = Cropper.pivotY model.cropper (Maybe.withDefault 0 (String.toFloat y)) }, Cmd.none )

        CropImage data ->
            ( { model | cropper = Cropper.init data }, Cmd.none )

        Crop crop ->
            ( { model | cropper = Cropper.crop model.cropper crop }, Cmd.none )

        ExportImage ->
            ( model, cropperData (Cropper.cropData model.cropper) )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ header []
            [ h1 [] [ a [ href "index.html" ] [ text "Elm Cropper Example" ] ]
            , p []
                [ text "Here is an image of "
                , strong [] [ text "some cats" ]
                , text ". If you do not like cats then "
                , button [ onClick <| CropImage { url = "assets/little-girl-1920-1280.jpg", crop = { width = 1080, height = 608 } } ] [ text "try this instead." ]
                ]
            , p []
                [ text "Here are other sizes to crop to:"
                , button [ onClick <| Crop { width = 240, height = 160 } ] [ text "240×160" ]
                , button [ onClick <| Crop { width = 640, height = 480 } ] [ text "640×480" ]
                , button [ onClick <| Crop { width = 820, height = 312 } ] [ text "820×312" ]
                , button [ onClick <| Crop { width = 1080, height = 608 } ] [ text "1080×608" ]
                ]
            , p [ class "info" ] [ span [] [ text "You can use the sliders below to zoom or position the image. Also try dragging it. Clicking \"Export\" sends the crop data to javascript where the cropped image is created using canvas..." ] ]
            ]
        , sourceInfoItems model.cropper
        , Cropper.view model.cropper |> Html.map ToCropper
        , cropInfoItems model.cropper
        , div [ class "controls" ]
            [ p [ class "controls__row" ]
                [ label [] [ text "Z" ]
                , input [ onInput Zoom, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (String.fromFloat model.cropper.zoom) ] []
                , span [] [ text <| showRound 4 model.cropper.zoom ]
                ]
            , p [ class "controls__row" ]
                [ label [] [ text "X" ]
                , input [ onInput PivotX, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (String.fromFloat model.cropper.pivot.x) ] []
                , label [] [ text "Y" ]
                , input [ onInput PivotY, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (String.fromFloat model.cropper.pivot.y) ] []
                ]
            , button [ class "controls__button", onClick <| ExportImage ] [ text "Crop" ]
            ]
        , p []
            [ text "Following link sets up the cropper "
            , a [ href "?s=assets/test-1920x1200.png&w=320&h=240" ] [ text "from javascript" ]
            , text "."
            ]
        , p [ class "info" ] [ span [] [ text "If an image is smaller than the crop size the cropper \"fails\" silently and the crop size is set to image size." ] ]
        , p []
            [ text "Here is an "
            , a [ href "?s=assets/true-potus-611x640.jpg&w=900&h=900" ] [ text "image too small to be cropped" ]
            , text "."
            ]
        ]


showRound : Int -> Float -> String
showRound d value =
    let
        f =
            modBy (10 ^ d) (round (value * toFloat (10 ^ d)))
    in
    String.fromInt (floor value) ++ "." ++ (String.padLeft d '0' <| String.fromInt f)


sourceInfoItems : Cropper.Model -> Html Msg
sourceInfoItems model =
    case model.image of
        Nothing ->
            div [ class "info-bar" ] []

        Just image ->
            div [ class "info-bar", style "max-width" (String.fromInt model.crop.width ++ "px") ]
                [ span [] [ "W: " ++ String.fromInt image.width |> text ]
                , span [] [ "H: " ++ String.fromInt image.height |> text ]
                , span [ class "fill" ] [ "SRC: " ++ image.src |> text ]
                ]


cropInfoItems : Cropper.Model -> Html Msg
cropInfoItems model =
    div [ class "info-bar", style "max-width" (String.fromInt model.crop.width ++ "px") ]
        [ span [] [ "W: " ++ showRound 2 (Cropper.imageSize model).x |> text ]
        , span [] [ "H: " ++ showRound 2 (Cropper.imageSize model).y |> text ]
        , span [] [ "X: " ++ String.fromInt (floor (Cropper.cropOrigin model).x) |> text ]
        , span [] [ "Y: " ++ String.fromInt (floor (Cropper.cropOrigin model).y) |> text ]
        ]
