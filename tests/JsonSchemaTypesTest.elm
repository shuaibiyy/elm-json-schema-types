module JsonSchemaTypesTest exposing (..)

import Test exposing (..)
import Expect
import Dict
import Json.Decode exposing (decodeString)
import JsonSchemaTypes exposing (..)
import JsonSchemaTypes.Decoders exposing (objectDecoder, primitiveDecoder)


multiPropSchema : String
multiPropSchema =
    """
    {
      "type": "object",
      "properties": {
        "source_warehouse": {
          "type": "string",
          "default": "42"
        },
        "order_number": {
          "type": "number"
        }
      },
      "required": ["source_warehouse", "order_number"]
    }
    """


simplePropSchema : String
simplePropSchema =
    """
    {
      "type": "string",
      "default": "42"
    }
    """


objectResToProps : Result String Object -> Properties
objectResToProps res =
    case res of
        Ok obj ->
            obj.properties

        _ ->
            Dict.empty


primitiveResToMaybe : Result String Primitive -> Maybe PropertyBody
primitiveResToMaybe res =
    case res of
        Ok sgl ->
            Just <| PrimitiveType sgl

        _ ->
            Nothing


primitiveGetTest : Test
primitiveGetTest =
    test "A primitive property can be retrieved" <|
        \() ->
            let
                multipleProps =
                    objectResToProps <| decodeString objectDecoder multiPropSchema

                expected =
                    primitiveResToMaybe <| decodeString primitiveDecoder simplePropSchema

                actual =
                    getProperty "source_warehouse" multipleProps
            in
                Expect.equal expected actual


all : Test
all =
    describe "JSON Schema suite"
        [ describe "Schema tests"
            [ primitiveGetTest
            ]
        ]
