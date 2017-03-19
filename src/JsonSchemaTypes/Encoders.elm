module JsonSchemaTypes.Encoders
    exposing
        ( primitiveEncoder
        , objectEncoder
        , arrayEncoder
        , propertyEncoder
        , propertiesEncoder
        )

{-| Encoders for Json schema types.

# Encoders
@docs primitiveEncoder, objectEncoder, arrayEncoder, propertyEncoder, propertiesEncoder
-}

import Dict
import Json.Encode exposing (object, int, float, string, bool, list, null, Value)
import JsonSchemaTypes exposing (..)


stringList : List String -> Value
stringList xs =
    list <| List.map (\x -> string x) xs


maybeBool : Maybe Bool -> Value
maybeBool x =
    bool <| Maybe.withDefault True x


maybeString : Maybe String -> Value
maybeString x =
    string <| Maybe.withDefault "" x


{-| Encode data of type `Single`.
-}
primitiveEncoder : Primitive -> Value
primitiveEncoder prim =
    object
        [ ( "__type__", string prim.type_ )
        , ( "default", maybeString prim.default )
        ]


{-| Encode data of type `Boolean`.
-}
boolEncoder : Primitive -> Value
boolEncoder bool =
    object
        [ ( "type", string bool.type_ )
        , ( "default", maybeString bool.default )
        ]


{-| Encode data of type `Array`.
-}
arrayEncoder : Array -> Value
arrayEncoder array =
    object
        [ ( "type", string array.type_ )
        , ( "items", propertyEncoder array.items )
        , ( "minItems", int array.minItems )
        , ( "maxItems", int array.maxItems )
        ]


{-| Encode data of type `Property`.
-}
propertyEncoder : PropertyBody -> Value
propertyEncoder prop =
    case prop of
        PrimitiveType prim ->
            case prim.type_ of
                "boolean" ->
                    boolEncoder prim

                _ ->
                    primitiveEncoder prim

        ObjectType obj ->
            objectEncoder obj

        ArrayType array ->
            arrayEncoder array


{-| Encode data of type `Properties`.
-}
propertiesEncoder : Properties -> Value
propertiesEncoder props =
    Dict.toList props
        |> (List.map (\( k, v ) -> ( k, propertyEncoder v )))
        |> object


requiredEncoder : Maybe (List String) -> Value
requiredEncoder required =
    list <|
        List.map (\x -> string x) <|
            Maybe.withDefault [] required


{-| Encode data of type `Object`.
-}
objectEncoder : Object -> Value
objectEncoder { properties, required, additionalProperties } =
    object
        [ ( "properties", propertiesEncoder properties )
        , ( "type", string "object" )
        ]
