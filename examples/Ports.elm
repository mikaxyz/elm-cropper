port module Ports exposing (..)

import Cropper


type alias Point =
    { x : Int
    , y : Int
    }


type alias Rect =
    { width : Int
    , height : Int
    }


type alias ImageData =
    { url : String
    , crop : Rect
    }


port initWithImage : (ImageData -> msg) -> Sub msg


type alias CropData =
    { url : String
    , size : Rect
    , crop : Rect
    , resized : Rect
    , origin : Point
    }


port cropData : CropData -> Cmd msg


createCropData : Cropper.Model -> CropData
createCropData model =
    case (model.image) of
        Nothing ->
            CropData "" { width = 0, height = 0 } { width = 0, height = 0 } { width = 0, height = 0 } { x = 0, y = 0 }

        Just image ->
            let
                size =
                    Cropper.imageSize model

                origin =
                    Cropper.cropOrigin model
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
