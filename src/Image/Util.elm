module Image.Util exposing (..)

import Image.Types exposing (..)
import Util.Debug exposing (..)


initialModel : Image.Types.Image
initialModel =
    { imageUrl = "/assets/30192_1600x1200-4-cute-cats.jpg"
    , crop =
        { width = 820
        , height = 312
        }
    , zoom = 0.0
    , naturalSize =
        { width = 1600
        , height = 1200
        }
    , pivot =
        { x = 0.5
        , y = 0.5
        }
    }


type alias Vector =
    { x : Float
    , y : Float
    }


imageRatio { crop, naturalSize } =
    Vector
        (toFloat naturalSize.width / toFloat crop.width)
        (toFloat naturalSize.height / toFloat crop.height)


imageSize { crop, naturalSize, zoom } =
    let
        ratio =
            debugOff "imageRatio" <| imageRatio { crop = crop, naturalSize = naturalSize }

        ratioMin =
            debugOff "ratioMin" <| Basics.min ratio.x ratio.y

        minWidth =
            debugOff "minWidth" <| toFloat crop.width * (ratio.x / ratioMin)

        minHeight =
            debugOff "minHeight" <| toFloat crop.height * (ratio.y / ratioMin)

        width =
            minWidth + ((toFloat naturalSize.width - minWidth) * zoom)

        height =
            minHeight + ((toFloat naturalSize.height - minHeight) * zoom)
    in
        debugOff "imageSize" <| Vector width height
