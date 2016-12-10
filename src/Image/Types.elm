module Image.Types exposing (..)


type alias Pivot =
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
    , pivot : Pivot
    }
