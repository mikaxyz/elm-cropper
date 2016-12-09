port module Main exposing (..)

import Test exposing (..)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Tests.Cropper.Image


main : TestProgram
main =
    [ Tests.Cropper.Image.all
    ]
        |> Test.concat
        |> run emit


port emit : ( String, Value ) -> Cmd msg
