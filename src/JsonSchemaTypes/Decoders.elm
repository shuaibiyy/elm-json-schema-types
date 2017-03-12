module JsonSchemaTypes.Decoders
    exposing
        ( singleDecoder
        , objectDecoder
        , arrayDecoder
        , propertyDecoder
        , propertiesDecoder
        )

{-| Decoders for Json schema types.

# Decoders
@docs singleDecoder, objectDecoder, arrayDecoder, propertyDecoder, propertiesDecoder
-}

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required, optional)
import JsonSchemaTypes exposing (..)


maybe : String -> Decoder (Maybe a) -> Decoder (Maybe a -> b) -> Decoder b
maybe key maybeDecoder =
    optional key maybeDecoder Nothing


customDecoder : Decoder b -> (b -> Result String a) -> Decoder a
customDecoder decoder toResult =
    andThen
        (\a ->
            case toResult a of
                Ok b ->
                    succeed b

                Err err ->
                    fail err
        )
        decoder


stringify : a -> Result String String
stringify v =
    Ok (toString v)


stringifyAllTheTypes : Decoder String
stringifyAllTheTypes =
    oneOf
        [ string
        , customDecoder int stringify
        , customDecoder bool stringify
        ]


{-| Decode data of type `Single`.
-}
singleDecoder : Decoder Single
singleDecoder =
    decode Single
        |> required "type" string
        |> maybe "default" (nullable stringifyAllTheTypes)
        |> maybe "pattern" (nullable stringifyAllTheTypes)


{-| Decode data of type `Object`.
-}
objectDecoder : Decode.Decoder Object
objectDecoder =
    decode Object
        |> required "properties" (Decode.lazy (\_ -> propertiesDecoder))
        |> maybe "required" (nullable (list string))
        |> maybe "additionalProperties" (nullable bool)


{-| Decode data of type `Array`.
-}
arrayDecoder : Decode.Decoder Array
arrayDecoder =
    decode Array
        |> required "type" string
        |> required "items" (Decode.lazy (\_ -> propertyDecoder))
        |> optional "minItems" int 0
        |> optional "maxItems" int 0
        |> maybe "uniqueItems" (nullable bool)
        |> maybe "additionalItems" (nullable bool)


{-| Decode data of type `PropertyBody`.
-}
propertyDecoder : Decode.Decoder PropertyBody
propertyDecoder =
    oneOf
        [ map Compound (Decode.lazy (\_ -> objectDecoder))
        , map Collection arrayDecoder
        , map Simple singleDecoder
        ]


{-| Decode data of type `Properties`.
-}
propertiesDecoder : Decode.Decoder Properties
propertiesDecoder =
    dict <|
        oneOf
            [ map Compound (Decode.lazy (\_ -> objectDecoder))
            , map Collection arrayDecoder
            , map Simple singleDecoder
            ]
