module JsonSchemaTypes.DecodersTest exposing (..)

import Test exposing (..)
import Dict
import Expect
import String
import Json.Decode
import JsonSchemaTypes exposing (PropertyBody(..))
import JsonSchemaTypes.Decoders exposing (objectDecoder)


success : Result a b -> Bool
success result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False


simpleSchema : String
simpleSchema =
    """
    {
      "properties": {
        "source_warehouse": {
          "type": "number",
          "default": 42
        }
      }
    }
    """


multiPropSchema : String
multiPropSchema =
    """
    {
      "type": "object",
      "properties": {
        "source_warehouse": {
          "type": "string",
          "pattern": "[a-zA-Z0-9]{9}$"
        },
        "order_number": {
          "type": "number"
        }
      },
      "required": ["source_warehouse", "order_number"]
    }
    """


nestedSchema : String
nestedSchema =
    """
    {
      "type": "object",
      "properties": {
        "data": {
          "type": "object",
          "additionalProperties": true,
          "properties": {
            "attach_return_label": {
              "type": "boolean"
            },
            "shipping_country": {
              "type": "string",
              "pattern": "^(tandy|Miller)([a-zA-Z0-9]{6})(carol)$"
            }
          },
          "required": []
        },
        "data_type": {
          "type": "string",
          "pattern": "^(tandy|Miller)([a-zA-Z0-9]{6})(carol)$"
        },
        "data_op": {
          "type": "string"
        }
      },
      "required": [
        "data",
        "data_type"
      ],
      "additionalProperties": false
    }
    """


arraySchema : String
arraySchema =
    """
    {
      "type": "object",
      "properties": {
        "items": {
          "type": "array",
          "minItems": 5,
          "maxItems": 5,
          "additionalItems": false,
          "items": {
            "type": "object",
            "additionalProperties": true,
            "properties": {
              "size": {
                "type": "string"
              },
              "description": {
                "type": "string"
              }
            }
          }
        }
      }
    }
    """


arraySchemaTest : Test
arraySchemaTest =
    test "Schemas with array properties are decoed correctly" <|
        \() ->
            let
                decodedOutput =
                    Json.Decode.decodeString
                        objectDecoder
                        arraySchema
            in
                Expect.equal decodedOutput
                    (Ok
                        { properties =
                            Dict.fromList
                                [ ( "items"
                                  , Collection
                                        { type_ = "array"
                                        , minItems = 5
                                        , maxItems = 5
                                        , uniqueItems = Nothing
                                        , additionalItems = Just False
                                        , items =
                                            (Compound
                                                { properties =
                                                    Dict.fromList
                                                        [ ( "description"
                                                          , Simple
                                                                { type_ = "string"
                                                                , default = Nothing
                                                                , pattern = Nothing
                                                                }
                                                          )
                                                        , ( "size"
                                                          , Simple
                                                                { type_ = "string"
                                                                , default = Nothing
                                                                , pattern = Nothing
                                                                }
                                                          )
                                                        ]
                                                , required = Nothing
                                                , additionalProperties = Just True
                                                }
                                            )
                                        }
                                  )
                                ]
                        , required = Nothing
                        , additionalProperties = Nothing
                        }
                    )


simpleSchemaTest : Test
simpleSchemaTest =
    test "Schemas with a single property are decoded correctly" <|
        \() ->
            let
                decodedOutput =
                    Json.Decode.decodeString
                        objectDecoder
                        simpleSchema
            in
                Expect.equal decodedOutput
                    (Ok
                        { properties =
                            Dict.fromList
                                [ ( "source_warehouse"
                                  , Simple
                                        { type_ = "number"
                                        , default = Just "42"
                                        , pattern = Nothing
                                        }
                                  )
                                ]
                        , required = Nothing
                        , additionalProperties = Nothing
                        }
                    )


multiPropsSchemaTest : Test
multiPropsSchemaTest =
    test "Schemas with multiples properties are decoded correctly" <|
        \() ->
            let
                decodedOutput =
                    Json.Decode.decodeString
                        objectDecoder
                        multiPropSchema
            in
                Expect.equal decodedOutput
                    (Ok
                        { properties =
                            Dict.fromList
                                [ ( "order_number"
                                  , Simple
                                        { type_ = "number"
                                        , default = Nothing
                                        , pattern = Nothing
                                        }
                                  )
                                , ( "source_warehouse"
                                  , Simple
                                        { type_ = "string"
                                        , default = Nothing
                                        , pattern = Just "[a-zA-Z0-9]{9}$"
                                        }
                                  )
                                ]
                        , required = Just [ "source_warehouse", "order_number" ]
                        , additionalProperties = Nothing
                        }
                    )


nestedPropsSchemaTest : Test
nestedPropsSchemaTest =
    test "Schemas with nested properties are decoded correctly" <|
        \() ->
            let
                decodedOutput =
                    Json.Decode.decodeString
                        objectDecoder
                        nestedSchema
            in
                Expect.equal decodedOutput
                    (Ok
                        { properties =
                            Dict.fromList
                                [ ( "data_op"
                                  , Simple
                                        { type_ = "string"
                                        , default = Nothing
                                        , pattern = Nothing
                                        }
                                  )
                                , ( "data_type"
                                  , Simple
                                        { type_ = "string"
                                        , default = Nothing
                                        , pattern = Just "^(tandy|Miller)([a-zA-Z0-9]{6})(carol)$"
                                        }
                                  )
                                , ( "data"
                                  , Compound
                                        { properties =
                                            Dict.fromList
                                                [ ( "shipping_country"
                                                  , Simple
                                                        { type_ = "string"
                                                        , default = Nothing
                                                        , pattern = Just "^(tandy|Miller)([a-zA-Z0-9]{6})(carol)$"
                                                        }
                                                  )
                                                , ( "attach_return_label"
                                                  , Simple
                                                        { type_ = "boolean"
                                                        , default = Nothing
                                                        , pattern = Nothing
                                                        }
                                                  )
                                                ]
                                        , required = Just []
                                        , additionalProperties = Just True
                                        }
                                  )
                                ]
                        , required = Just [ "data", "data_type" ]
                        , additionalProperties = Just False
                        }
                    )


all : Test
all =
    describe "JSON Schema Decoders suite"
        [ describe "Decoder tests"
            [ simpleSchemaTest
            , multiPropsSchemaTest
            , nestedPropsSchemaTest
            , arraySchemaTest
            ]
        ]
