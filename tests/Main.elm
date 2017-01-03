port module Main exposing (..)

import Test exposing (..)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Test.Cropper.Helper


main : TestProgram
main =
    [ Test.Cropper.Helper.all
    ]
        |> Test.concat
        |> run emit


port emit : ( String, Value ) -> Cmd msg
