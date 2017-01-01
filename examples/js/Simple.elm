module Simple exposing (..)

import Html exposing (..)
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



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ Cropper.view model.cropper |> Html.map ToCropper ]
