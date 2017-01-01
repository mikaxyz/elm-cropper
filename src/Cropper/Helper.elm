module Cropper.Helper exposing (..)

import Cropper.Types as Types exposing (..)


imageRatio : { a | image : Image, crop : Rect } -> Vector
imageRatio { image, crop } =
    Vector
        (toFloat image.width / toFloat crop.width)
        (toFloat image.height / toFloat crop.height)


imageSize : { a | image : Image, crop : Rect, zoom : Float } -> Vector
imageSize { image, crop, zoom } =
    let
        ratio =
            imageRatio { crop = crop, image = image }

        ratioMin =
            Basics.min ratio.x ratio.y

        minWidth =
            toFloat crop.width * (ratio.x / ratioMin)

        minHeight =
            toFloat crop.height * (ratio.y / ratioMin)

        width =
            minWidth + ((toFloat image.width - minWidth) * zoom)

        height =
            minHeight + ((toFloat image.height - minHeight) * zoom)
    in
        Vector width height


cropOrigin : { a | image : Image, pivot : Vector, crop : Rect, zoom : Float } -> Vector
cropOrigin { crop, pivot, zoom, image } =
    let
        size =
            imageSize { image = image, crop = crop, zoom = zoom }

        x =
            pivot.x * (size.x - toFloat crop.width)

        y =
            pivot.y * (size.y - toFloat crop.height)
    in
        Vector x y
