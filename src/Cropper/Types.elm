module Cropper.Types exposing (..)


type alias Rect =
    { width : Int
    , height : Int
    }


type alias Image =
    { src : String
    , width : Int
    , height : Int
    }
