module Image.Types exposing (..)


type alias Vector =
    { x : Float
    , y : Float
    }


type alias Box =
    { width : Int
    , height : Int
    }


type alias Image =
    { imageUrl : String
    , crop : Box
    , zoom : Float
    , naturalSize : Box
    , pivot : Vector
    }


type CropperImage
    = Unset Image
    | Loading Image
    | Loaded Image
