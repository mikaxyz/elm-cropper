port module Cropper.Ports exposing (cropperWithImage, cropperData)

{-| Ports for Elm Cropper
@docs cropperWithImage, cropperData
-}

import Cropper.Types exposing (Model, ImageData, CropData)


port cropperWithImage : (ImageData -> msg) -> Sub msg


port cropperData : CropData -> Cmd msg
