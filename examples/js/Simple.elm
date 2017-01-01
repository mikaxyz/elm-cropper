module Simple exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Cropper


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { cropper =
            Cropper.init
                { url = "/assets/kittens-1280x711.jpg"
                , crop = { width = 720, height = 480 }
                }
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ToCropper (Cropper.subscriptions model.cropper)
        ]


type Msg
    = ToCropper Cropper.Msg
    | Zoom String
    | PivotX String
    | PivotY String



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
            ( { model | cropper = Cropper.zoom model.cropper (Result.withDefault 0 (String.toFloat zoom)) }, Cmd.none )

        PivotX x ->
            ( { model | cropper = Cropper.pivotX model.cropper (Result.withDefault 0 (String.toFloat x)) }, Cmd.none )

        PivotY y ->
            ( { model | cropper = Cropper.pivotY model.cropper (Result.withDefault 0 (String.toFloat y)) }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Cropper.view model.cropper |> Html.map ToCropper
        , div [ class "controls" ]
            [ p [ class "controls__row" ]
                [ label [] [ text "Z" ]
                , input [ onInput Zoom, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (toString model.cropper.zoom) ] []
                , span [] [ text <| showRound 4 model.cropper.zoom ]
                ]
            , p [ class "controls__row" ]
                [ label [] [ text "X" ]
                , input [ onInput PivotX, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (toString model.cropper.pivot.x) ] []
                , label [] [ text "Y" ]
                , input [ onInput PivotY, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (toString model.cropper.pivot.y) ] []
                ]
            ]
        ]


showRound : Int -> Float -> String
showRound d value =
    let
        f =
            (round (value * toFloat (10 ^ d))) % (10 ^ d)
    in
        toString (floor value) ++ "." ++ (String.padLeft d '0' <| toString f)
