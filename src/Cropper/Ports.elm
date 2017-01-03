port module Cropper.Ports exposing (createCropData, cropperWithImage, cropperData)

{-| Ports for Elm Cropper
@docs createCropData
@docs cropperWithImage, cropperData
-}

import Cropper.Types exposing (..)
import Cropper exposing (..)


port cropperWithImage : (ImageData -> msg) -> Sub msg


port cropperData : CropData -> Cmd msg


type alias Point =
    { x : Int
    , y : Int
    }


type alias ImageData =
    { url : String
    , crop : Rect
    }


type alias CropData =
    { url : String
    , size : Rect
    , crop : Rect
    , resized : Rect
    , origin : Point
    }


{-| Get crop data
-}
createCropData : Cropper.Model -> CropData
createCropData model =
    case (model.image) of
        Nothing ->
            CropData "" { width = 0, height = 0 } { width = 0, height = 0 } { width = 0, height = 0 } { x = 0, y = 0 }

        Just image ->
            let
                size =
                    imageSize model

                origin =
                    cropOrigin model
            in
                { url = image.src
                , size =
                    { width = image.width
                    , height = image.height
                    }
                , crop = model.crop
                , resized =
                    { width = round size.x
                    , height = round size.y
                    }
                , origin =
                    { x = round origin.x
                    , y = round origin.y
                    }
                }
