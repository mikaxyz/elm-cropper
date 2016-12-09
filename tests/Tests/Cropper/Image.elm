module Tests.Cropper.Image exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Cropper.Image as Image


m =
    Image.initialModel


model : Image.Model
model =
    { m
        | zoom = 1.0
        , crop =
            { width = 820
            , height = 400
            }
        , naturalSize =
            { width = 1600
            , height = 1200
            }
    }


all : Test
all =
    describe "Cropper Test Suite"
        [ describe "Image"
            [ test "calculates correct size at min zoom" <|
                \() ->
                    Expect.equal
                        (Image.imageWidth
                            { model
                                | zoom = 0.0
                            }
                        )
                        820
            , test "calculates correct size at medium zoom" <|
                \() ->
                    Expect.equal
                        (Image.imageWidth
                            { model
                                | zoom = 0.5
                            }
                        )
                        1200
            , test "calculates correct size at max zoom" <|
                \() ->
                    Expect.equal
                        (Image.imageWidth
                            { model
                                | zoom = 1.0
                            }
                        )
                        1600
            ]
        ]
