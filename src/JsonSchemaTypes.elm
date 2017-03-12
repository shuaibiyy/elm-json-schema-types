module JsonSchemaTypes
    exposing
        ( PropertyBody(..)
        , Single
        , Object
        , Array
        , Properties
        , getProperty
        , updateProperties
        )

{-| Json Schema Types in Elm

# Type constructors
@docs PropertyBody, Single, Object, Array, Properties

# Common Helpers
@docs getProperty, updateProperties
-}

import Dict exposing (Dict)


{-| Path refers to the absolute key of a property, which can be understood as the path to it.
    For example, a key with the value `data.shipment_number` means the `shipment_number` property
    can be found in a `data` object. A path can be used to fetch a property from a dictionary of
    properties. See `getProperty` in `Schema/State.elm`.
-}
type alias Path =
    String


{-| Represents the possible JSON schema types.
-}
type PropertyBody
    = Simple Single
    | Compound Object
    | Collection Array


{-| Represents either Number, String or Boolean types.
-}
type alias Single =
    { type_ : String
    , default : Maybe String
    , pattern : Maybe String
    }


{-| Represents Object types.
-}
type alias Object =
    { properties : Properties
    , required : Maybe (List String)
    , additionalProperties : Maybe Bool
    }


{-| Represents Array types.
-}
type alias Array =
    { type_ : String
    , items : PropertyBody
    , minItems : Int
    , maxItems : Int
    , uniqueItems : Maybe Bool
    , additionalItems : Maybe Bool
    }


{-| A dictionary containing properties belonging to a schema object.
-}
type alias Properties =
    Dict String PropertyBody


defaultSchema : Object
defaultSchema =
    { properties = Dict.empty
    , required = Nothing
    , additionalProperties = Nothing
    }


pathSeparator : String
pathSeparator =
    "."


splitPath : Path -> List String
splitPath path =
    String.split pathSeparator path


concatPath : List String -> Path
concatPath paths =
    String.join pathSeparator paths


removeFromList : Int -> List a -> List a
removeFromList idx xs =
    (List.take idx xs) ++ (List.drop (idx + 1) xs)


updateInList : Int -> a -> List a -> List a
updateInList idx x xs =
    (List.take idx xs) ++ [ x ] ++ (List.drop (idx + 1) xs)


{-|
    Retrieve a property from properties using a path e.g. "data.order_number",
    where properties is a dictionary of strings to properties.
-}
getProperty : Path -> Properties -> Maybe PropertyBody
getProperty absPath dict =
    let
        subPaths =
            splitPath absPath
    in
        if List.isEmpty subPaths then
            Nothing
        else
            case (List.head subPaths) of
                Just path ->
                    case (Dict.get path dict) of
                        Just w ->
                            case w of
                                {--- We return the `Simple` since it can't be composed of more properties --}
                                Simple s ->
                                    Just w

                                Compound c ->
                                    getProperty (concatPath <| List.drop 1 subPaths) c.properties

                                Collection cl ->
                                    {--- If the path has been exhausted, return the collection --}
                                    if (List.length subPaths) == 1 then
                                        Just w
                                    else
                                        getProperty (concatPath <| List.drop 1 subPaths) <|
                                            Dict.singleton "items" cl.items

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing


updateProp : List String -> PropertyBody -> String -> PropertyBody -> PropertyBody
updateProp paths newProp k v =
    let
        subPath =
            List.head paths

        updatedProp =
            case subPath of
                Just key ->
                    {--- If this is the value we seek based on the current path,
                         We return the value. --}
                    if k == key then
                        case v of
                            {--- Leaf path --}
                            Simple s ->
                                newProp

                            Compound c ->
                                let
                                    cProps =
                                        Dict.map (updateProp (List.drop 1 paths) newProp) c.properties
                                in
                                    Compound { c | properties = cProps }

                            Collection cl ->
                                {--- If the path has been exhausted, update the collection --}
                                if (List.length paths) == 1 then
                                    newProp
                                else
                                    let
                                        clProps =
                                            Dict.map (updateProp (List.drop 1 paths) newProp) <|
                                                Dict.singleton "items" cl.items
                                    in
                                        case (Dict.get "items" clProps) of
                                            Just x ->
                                                Collection { cl | items = x }

                                            Nothing ->
                                                v
                    else
                        v

                Nothing ->
                    v
    in
        updatedProp


{-|
    `Properties` is a dictionary of String to PropertyBody. To update an entry in the dictionary:
    We map over the dictionary; if the path we are currently on is found and is a leaf (`Simple`)
    as determined based on the current path, we simply replace the map entry with our new property.
    If it is a `Compound`, we recurse over its properties using the `updateProp` function.
-}
updateProperties : Path -> Properties -> PropertyBody -> Properties
updateProperties absPath props newProp =
    Dict.map (updateProp (splitPath absPath) newProp) props
