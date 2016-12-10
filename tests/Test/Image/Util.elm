module Test.Image.Util exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Image.Types
import Image.Util exposing (..)


model : Image.Types.Image
model =
    { initialModel
        | zoom = 1.0
        , crop =
            { width = 800
            , height = 300
            }
        , naturalSize =
            { width = 1600
            , height = 1200
            }
    }


all : Test
all =
    describe "ImageCropper module"
        [ describe "Image.Util.imageSize"
            [ test "landscape image size at min zoom" <|
                \() ->
                    Expect.equal
                        (Image.Util.imageSize
                            { model
                                | zoom = 0.0
                            }
                        )
                        { x = 800, y = 600 }
            , test "landscape image size at medium zoom" <|
                \() ->
                    Expect.equal
                        (Image.Util.imageSize
                            { model
                                | zoom = 0.5
                            }
                        )
                        { x = 1200, y = 900 }
            , test "landscape image size at max zoom" <|
                \() ->
                    Expect.equal
                        (Image.Util.imageSize
                            { model
                                | zoom = 1.0
                            }
                        )
                        { x = 1600, y = 1200 }
            ]
        ]
