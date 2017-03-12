module JsonSchemaTypes.Encoders
    exposing
        ( singleEncoder
        , objectEncoder
        , arrayEncoder
        , propertyEncoder
        , propertiesEncoder
        )

{-| Encoders for Json schema types.

# Encoders
@docs singleEncoder, objectEncoder, arrayEncoder, propertyEncoder, propertiesEncoder
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
singleEncoder : Single -> Value
singleEncoder single =
    object
        [ ( "__type__", string single.type_ )
        , ( "default", maybeString single.default )
        ]


{-| Encode data of type `Boolean`.
-}
boolEncoder : Single -> Value
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
        Simple single ->
            case single.type_ of
                "boolean" ->
                    boolEncoder single

                _ ->
                    singleEncoder single

        Compound obj ->
            objectEncoder obj

        Collection array ->
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
