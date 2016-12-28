module Image.Util exposing (..)

import Image.Types exposing (..)


initialModel : Image.Types.Image
initialModel =
    { imageUrl = ""
    , crop =
        { width = 820
        , height = 312
        }
    , zoom = 0.0
    , naturalSize =
        { width = 0
        , height = 0
        }
    , pivot =
        { x = 0.5
        , y = 0.5
        }
    }


imageRatio : { c | crop : Box, naturalSize : Box } -> Vector
imageRatio { crop, naturalSize } =
    Vector
        (toFloat naturalSize.width / toFloat crop.width)
        (toFloat naturalSize.height / toFloat crop.height)


imageSize : { c | crop : Box, naturalSize : Box, zoom : Float } -> Vector
imageSize { crop, naturalSize, zoom } =
    let
        ratio =
            imageRatio { crop = crop, naturalSize = naturalSize }

        ratioMin =
            Basics.min ratio.x ratio.y

        minWidth =
            toFloat crop.width * (ratio.x / ratioMin)

        minHeight =
            toFloat crop.height * (ratio.y / ratioMin)

        width =
            minWidth + ((toFloat naturalSize.width - minWidth) * zoom)

        height =
            minHeight + ((toFloat naturalSize.height - minHeight) * zoom)
    in
        Vector width height


cropOrigin : Image.Types.Image -> Vector
cropOrigin image =
    let
        size =
            imageSize image

        x =
            image.pivot.x * (size.x - toFloat image.crop.width)

        y =
            image.pivot.y * (size.y - toFloat image.crop.height)
    in
        Vector x y
