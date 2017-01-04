module Test.Cropper.Helper exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import DOM
import Cropper.Types exposing (Rect, Vector)
import Cropper exposing (..)


mock : { crop : Rect, imageSize : Rect } -> Model
mock { crop, imageSize } =
    { url = "testing.jpg"
    , crop = crop
    , image =
        Just
            { src = "testing.jpg"
            , width = imageSize.width
            , height = imageSize.height
            }
    , boundingClientRect = DOM.Rectangle 0 0 0 0
    , pivot = Vector 0.5 0.5
    , zoom = 0.0
    , drag = Nothing
    }


all : Test
all =
    describe "Cropper module"
        [ Test.concat
            [ imageSizeTest
            , cropOriginTest
            ]
        ]


cropOriginTest : Test
cropOriginTest =
    let
        model : Model
        model =
            mock
                { crop =
                    { width = 100
                    , height = 100
                    }
                , imageSize =
                    { width = 200
                    , height = 200
                    }
                }
    in
        describe "cropOrigin"
            -- TOP LEFT
            [ test "top left pivot at min zoom" <|
                \() ->
                    Expect.equal
                        (cropOrigin
                            { model
                                | zoom = 0.0
                                , pivot =
                                    { x = 0.0
                                    , y = 0.0
                                    }
                            }
                        )
                        { x = 0, y = 0 }
            , test "top left pivot at mid zoom" <|
                \() ->
                    Expect.equal
                        (cropOrigin
                            { model
                                | zoom = 0.5
                                , pivot =
                                    { x = 0.0
                                    , y = 0.0
                                    }
                            }
                        )
                        { x = 0, y = 0 }
            , test "top left pivot at max zoom" <|
                \() ->
                    Expect.equal
                        (cropOrigin
                            { model
                                | zoom = 1.0
                                , pivot =
                                    { x = 0.0
                                    , y = 0.0
                                    }
                            }
                        )
                        { x = 0, y = 0 }
              -- CENTER
            , test "center pivot at min zoom" <|
                \() ->
                    Expect.equal
                        (cropOrigin
                            { model
                                | zoom = 0.0
                                , pivot =
                                    { x = 0.5
                                    , y = 0.5
                                    }
                            }
                        )
                        { x = 0, y = 0 }
            , test "center pivot at mid zoom" <|
                \() ->
                    Expect.equal
                        (cropOrigin
                            { model
                                | zoom = 0.5
                                , pivot =
                                    { x = 0.5
                                    , y = 0.5
                                    }
                            }
                        )
                        { x = 25, y = 25 }
            , test "center pivot at max zoom" <|
                \() ->
                    Expect.equal
                        (cropOrigin
                            { model
                                | zoom = 1.0
                                , pivot =
                                    { x = 0.5
                                    , y = 0.5
                                    }
                            }
                        )
                        { x = 50, y = 50 }
              -- BOTTOM RIGHT
            , test "bottom right pivot at min zoom" <|
                \() ->
                    Expect.equal
                        (cropOrigin
                            { model
                                | zoom = 0.0
                                , pivot =
                                    { x = 1.0
                                    , y = 1.0
                                    }
                            }
                        )
                        { x = 0, y = 0 }
            , test "bottom right pivot at mid zoom" <|
                \() ->
                    Expect.equal
                        (cropOrigin
                            { model
                                | zoom = 0.5
                                , pivot =
                                    { x = 1.0
                                    , y = 1.0
                                    }
                            }
                        )
                        { x = 50, y = 50 }
            , test "bottom right pivot at max zoom" <|
                \() ->
                    Expect.equal
                        (cropOrigin
                            { model
                                | zoom = 1.0
                                , pivot =
                                    { x = 1.0
                                    , y = 1.0
                                    }
                            }
                        )
                        { x = 100, y = 100 }
            ]


imageSizeTest : Test
imageSizeTest =
    let
        model : Model
        model =
            mock
                { crop =
                    { width = 800
                    , height = 300
                    }
                , imageSize =
                    { width = 1600
                    , height = 1200
                    }
                }
    in
        describe "imageSize"
            [ test "landscape image size at min zoom" <|
                \() ->
                    Expect.equal
                        (imageSize
                            { model
                                | zoom = 0.0
                            }
                        )
                        { x = 800, y = 600 }
            , test "landscape image size at medium zoom" <|
                \() ->
                    Expect.equal
                        (imageSize
                            { model
                                | zoom = 0.5
                            }
                        )
                        { x = 1200, y = 900 }
            , test "landscape image size at max zoom" <|
                \() ->
                    Expect.equal
                        (imageSize
                            { model
                                | zoom = 1.0
                            }
                        )
                        { x = 1600, y = 1200 }
            , test "portrait image size at min zoom" <|
                \() ->
                    Expect.equal
                        (imageSize
                            { model
                                | zoom = 0.0
                                , crop =
                                    { width = 120
                                    , height = 360
                                    }
                            }
                        )
                        { x = 480, y = 360 }
            , test "portrait image size at mid zoom" <|
                \() ->
                    Expect.equal
                        (imageSize
                            { model
                                | zoom = 0.5
                                , crop =
                                    { width = 120
                                    , height = 360
                                    }
                            }
                        )
                        { x = 1040, y = 780 }
            , test "portrait image size at max zoom" <|
                \() ->
                    Expect.equal
                        (imageSize
                            { model
                                | zoom = 1.0
                                , crop =
                                    { width = 120
                                    , height = 360
                                    }
                            }
                        )
                        { x = 1600, y = 1200 }
            , test "crop square at min zoom" <|
                \() ->
                    Expect.equal
                        (imageSize
                            { model
                                | zoom = 0.0
                                , crop =
                                    { width = 300
                                    , height = 300
                                    }
                            }
                        )
                        { x = 400, y = 300 }
            , test "crop square at mid zoom" <|
                \() ->
                    Expect.equal
                        (imageSize
                            { model
                                | zoom = 0.5
                                , crop =
                                    { width = 300
                                    , height = 300
                                    }
                            }
                        )
                        { x = 1000, y = 750 }
            , test "crop square at max zoom" <|
                \() ->
                    Expect.equal
                        (imageSize
                            { model
                                | zoom = 1.0
                                , crop =
                                    { width = 300
                                    , height = 300
                                    }
                            }
                        )
                        { x = 1600, y = 1200 }
            ]
