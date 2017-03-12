port module Main exposing (..)

import Test
import JsonSchemaTypes.DecodersTest as DecodersTest
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    [ DecodersTest.all
    ]
        |> Test.concat
        |> run emit


port emit : ( String, Value ) -> Cmd msg
