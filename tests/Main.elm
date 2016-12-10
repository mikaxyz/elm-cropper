port module Main exposing (..)

import Test exposing (..)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Test.Image.Util


main : TestProgram
main =
    [ Test.Image.Util.all
    ]
        |> Test.concat
        |> run emit


port emit : ( String, Value ) -> Cmd msg
