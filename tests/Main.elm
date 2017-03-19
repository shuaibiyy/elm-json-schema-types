port module Main exposing (..)

import Test
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import JsonSchemaTypesTest
import JsonSchemaTypes.DecodersTest as DecodersTest

main : TestProgram
main =
    [ JsonSchemaTypesTest.all
     , DecodersTest.all
    ]
        |> Test.concat
        |> run emit


port emit : ( String, Value ) -> Cmd msg
