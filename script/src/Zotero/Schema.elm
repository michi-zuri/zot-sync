module Zotero.Schema exposing
    ( Schema, ItemType, Field
    , decoder
    , baseOf, baseFields, fieldNamesForBase
    )

{-| The Zotero item schema served at <https://api.zotero.org/schema>.

Only the parts zot-sync needs are decoded. Decoding is lenient: an entry that
does not have the expected shape is skipped instead of failing the whole
schema, so a surprising schema change can never stop a sync.

@docs Schema, ItemType, Field
@docs decoder
@docs baseOf, baseFields, fieldNamesForBase

-}

import Json.Decode as Decode exposing (Decoder)
import Set


{-| -}
type alias Schema =
    { version : Int
    , itemTypes : List ItemType
    }


{-| -}
type alias ItemType =
    { name : String
    , fields : List Field
    , creatorTypes : List String
    }


{-| A type-specific field. `baseField` is set when the field is a renamed
generic field, e.g. `caseName` for `case` items is based on `title`.
-}
type alias Field =
    { name : String
    , baseField : Maybe String
    }


{-| -}
decoder : Decoder Schema
decoder =
    Decode.map2 Schema
        (Decode.field "version" Decode.int)
        (Decode.field "itemTypes" (lenientList itemTypeDecoder))


itemTypeDecoder : Decoder ItemType
itemTypeDecoder =
    Decode.map3 ItemType
        (Decode.field "itemType" Decode.string)
        (optionalList "fields" fieldDecoder)
        (optionalList "creatorTypes" (Decode.field "creatorType" Decode.string))


fieldDecoder : Decoder Field
fieldDecoder =
    Decode.map2 Field
        (Decode.field "field" Decode.string)
        (Decode.maybe (Decode.field "baseField" Decode.string))


optionalList : String -> Decoder a -> Decoder (List a)
optionalList name itemDecoder =
    Decode.oneOf [ Decode.field name (lenientList itemDecoder), Decode.succeed [] ]


{-| Decode the elements that match, skip the ones that do not.
-}
lenientList : Decoder a -> Decoder (List a)
lenientList itemDecoder =
    Decode.list (Decode.maybe itemDecoder)
        |> Decode.map (List.filterMap identity)


{-| The generic field a type-specific field is stored under in Zotero.
-}
baseOf : Field -> String
baseOf field =
    Maybe.withDefault field.name field.baseField


{-| Every generic field name across all item types, sorted. These are the
columns of the wide `items` view, as in zot-sync 0.3's `items` table.
-}
baseFields : Schema -> List String
baseFields schema =
    schema.itemTypes
        |> List.concatMap (.fields >> List.map baseOf)
        |> Set.fromList
        |> Set.toList


{-| All field names that hold a given generic field, e.g. for `title`:
`title`, `caseName`, `nameOfAct`, `subject`, ... The generic name comes first.
An item only ever has one of them, which is what makes a COALESCE over them
correct.
-}
fieldNamesForBase : Schema -> String -> List String
fieldNamesForBase schema base =
    let
        specific : List String
        specific =
            schema.itemTypes
                |> List.concatMap .fields
                |> List.filter (\field -> field.baseField == Just base)
                |> List.map .name
                |> Set.fromList
                |> Set.remove base
                |> Set.toList

        hasGeneric : Bool
        hasGeneric =
            schema.itemTypes
                |> List.any (.fields >> List.any (\field -> field.name == base))
    in
    if hasGeneric || List.isEmpty specific then
        base :: specific

    else
        specific
