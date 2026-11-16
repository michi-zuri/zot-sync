module Db.Views exposing
    ( SqlType(..), Column, ExistingColumn, Input
    , plan, specs
    , existingColumnsQuery, existingColumnsDecoder, groupExisting
    , observedFieldsQuery, observedFieldsDecoder
    , typeName
    )

{-| Generates the query views in each library's schema (`zot_u_123`, ...).

The views keep zot-sync 0.3's names and columns (`items`, one view per item
type such as `"journalArticle"`, the same camelCase column names), so existing
queries keep working. They read from the raw JSON in `zotero.*`, which makes
them cheap to regenerate whenever Zotero's schema changes.

The planner never removes or reorders a column. It reads each view's current
columns from the catalog, keeps them in place and appends new ones, so
`CREATE OR REPLACE VIEW` always succeeds. That is exactly what zot-sync 0.3
got wrong: Zotero inserts new fields in the middle of an item type's field
list, which `CREATE OR REPLACE VIEW` refuses. Keeping columns in place also
means views you built on top of these keep working. A field Zotero removes
stays as a column and keeps reading whatever the item data still holds.

Column types are a fixed function of the field name, never of the schema, so
a schema change cannot change a column's type. Only if a later zot-sync
release changes a type is a view dropped and recreated.

@docs SqlType, Column, ExistingColumn, Input
@docs plan, specs
@docs existingColumnsQuery, existingColumnsDecoder, groupExisting
@docs observedFieldsQuery, observedFieldsDecoder
@docs typeName

-}

import Db.Sql as Sql exposing (Statement)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Set exposing (Set)
import Zotero.Library as Library exposing (Library)
import Zotero.Schema as Schema exposing (Schema)


{-| -}
type SqlType
    = Text
    | Integer
    | BigInt
    | Boolean
    | Jsonb
    | Timestamp
    | TimestampTz


{-| The name `format_type()` reports in the catalog.
-}
typeName : SqlType -> String
typeName sqlType =
    case sqlType of
        Text ->
            "text"

        Integer ->
            "integer"

        BigInt ->
            "bigint"

        Boolean ->
            "boolean"

        Jsonb ->
            "jsonb"

        Timestamp ->
            "timestamp without time zone"

        TimestampTz ->
            "timestamp with time zone"


castName : SqlType -> String
castName sqlType =
    case sqlType of
        Timestamp ->
            "timestamp"

        TimestampTz ->
            "timestamptz"

        _ ->
            typeName sqlType


parseType : String -> Maybe SqlType
parseType name =
    [ Text, Integer, BigInt, Boolean, Jsonb, Timestamp, TimestampTz ]
        |> List.filter (\t -> typeName t == name)
        |> List.head


{-| A view column: its name, declared type and the SQL producing it.
-}
type alias Column =
    { name : String
    , sqlType : SqlType
    , expression : String
    }


{-| A column as the catalog reports it. `Nothing` is a type zot-sync never
generates.
-}
type alias ExistingColumn =
    { name : String
    , sqlType : Maybe SqlType
    }


{-| -}
type alias Input =
    { library : Library
    , schema : Schema
    , observed : Dict String (List String)
    , existing : Dict String (List ExistingColumn)
    }


type alias Spec =
    { name : String
    , from : String
    , columns : List Column
    , retired : String -> SqlType -> String
    }


{-| The DDL that brings the library's views up to date.
-}
plan : Input -> List Statement
plan input =
    let
        schemaName : String
        schemaName =
            Library.schemaName input.library
    in
    Sql.statement ("CREATE SCHEMA IF NOT EXISTS " ++ Sql.identifier schemaName) []
        :: List.concatMap (planView schemaName input.existing) (viewSpecs input)


planView : String -> Dict String (List ExistingColumn) -> Spec -> List Statement
planView schemaName existing spec =
    case Dict.get spec.name existing of
        Nothing ->
            [ createView "CREATE VIEW" schemaName spec spec.columns ]

        Just current ->
            let
                desired : Dict String Column
                desired =
                    spec.columns |> List.map (\c -> ( c.name, c )) |> Dict.fromList

                currentNames : Set String
                currentNames =
                    current |> List.map .name |> Set.fromList

                conflicts : Bool
                conflicts =
                    current
                        |> List.any
                            (\e ->
                                case ( Dict.get e.name desired, e.sqlType ) of
                                    ( Just d, Just t ) ->
                                        d.sqlType /= t

                                    ( _, Nothing ) ->
                                        True

                                    ( Nothing, Just _ ) ->
                                        False
                            )

                kept : List Column
                kept =
                    current
                        |> List.map
                            (\e ->
                                case ( Dict.get e.name desired, e.sqlType ) of
                                    ( Just d, _ ) ->
                                        d

                                    ( Nothing, sqlType ) ->
                                        let
                                            t =
                                                Maybe.withDefault Text sqlType
                                        in
                                        { name = e.name, sqlType = t, expression = spec.retired e.name t }
                            )

                added : List Column
                added =
                    spec.columns |> List.filter (\c -> not (Set.member c.name currentNames))
            in
            if conflicts then
                [ Sql.statement ("DROP VIEW " ++ Sql.qualified schemaName spec.name) []
                , createView "CREATE VIEW" schemaName spec spec.columns
                ]

            else
                [ createView "CREATE OR REPLACE VIEW" schemaName spec (kept ++ added) ]


createView : String -> String -> Spec -> List Column -> Statement
createView verb schemaName spec columns =
    Sql.statement
        (verb
            ++ " "
            ++ Sql.qualified schemaName spec.name
            ++ " AS\nSELECT "
            ++ String.join ",\n       " (List.map selectItem columns)
            ++ "\nFROM "
            ++ spec.from
        )
        []


selectItem : Column -> String
selectItem column =
    "(" ++ column.expression ++ ")::" ++ castName column.sqlType ++ " AS " ++ Sql.identifier column.name


{-| The views a fresh plan would create, with their columns in order.
-}
specs : Input -> List { name : String, columns : List ( String, SqlType ) }
specs input =
    viewSpecs input |> List.map (\s -> { name = s.name, columns = List.map (\c -> ( c.name, c.sqlType )) s.columns })


viewSpecs : Input -> List Spec
viewSpecs input =
    let
        libraryId : String
        libraryId =
            Sql.literal (Library.id input.library)

        typeViews : List Spec
        typeViews =
            itemTypeNames input
                |> List.filter (\name -> not (List.member name auxiliaryNames))
                |> List.map (itemTypeSpec input libraryId)
    in
    itemsSpec input libraryId :: typeViews ++ auxiliarySpecs libraryId



-- ITEM VIEWS


itemTypeNames : Input -> List String
itemTypeNames input =
    let
        fromSchema =
            List.map .name input.schema.itemTypes

        seenOnly =
            Dict.keys input.observed |> List.filter (\t -> not (List.member t fromSchema))
    in
    fromSchema ++ seenOnly


itemTypeSpec : Input -> String -> String -> Spec
itemTypeSpec input libraryId name =
    let
        schemaFields : List String
        schemaFields =
            input.schema.itemTypes
                |> List.filter (\t -> t.name == name)
                |> List.concatMap (.fields >> List.map .name)

        leading : List Column
        leading =
            if name == "note" then
                -- zot-sync 0.3 left these out of the note view
                standardColumns |> List.filter (\c -> not (List.member c.name [ "parsedDate", "creatorSummary", "creators" ]))

            else
                standardColumns

        parent : List String
        parent =
            if List.member name childTypes then
                [ "parentItem" ]

            else
                []

        named : List String
        named =
            schemaFields ++ knownExtras name ++ parent

        covered : Set String
        covered =
            Set.union envelopeKeys (Set.fromList (List.map .name leading ++ named))

        seen : List String
        seen =
            Dict.get name input.observed
                |> Maybe.withDefault []
                |> List.filter (\field -> not (Set.member field covered))
                |> List.sort
    in
    { name = name
    , from = "zotero.item i\nWHERE i.library = " ++ libraryId ++ " AND i.item_type = " ++ Sql.literal name
    , columns = dedupe (leading ++ List.map fieldColumn (named ++ seen))
    , retired = retiredItemField
    }


{-| The wide `items` view: one column per generic field, like zot-sync 0.3's
`items` table, plus everything child items and unknown fields bring.
-}
itemsSpec : Input -> String -> Spec
itemsSpec input libraryId =
    let
        bases : List String
        bases =
            ("customJSON" :: Schema.baseFields input.schema)
                |> Set.fromList
                |> Set.toList

        allSchemaFields : Set String
        allSchemaFields =
            input.schema.itemTypes
                |> List.concatMap (.fields >> List.map .name)
                |> Set.fromList

        covered : Set String
        covered =
            envelopeKeys
                |> Set.union allSchemaFields
                |> Set.union (Set.fromList bases)
                |> Set.union (Set.fromList (List.map .name standardColumns))

        extras : List String
        extras =
            (List.concatMap knownExtras childTypes ++ List.concat (Dict.values input.observed))
                |> List.filter (\field -> not (Set.member field covered))
                |> Set.fromList
                |> Set.toList
                |> (::) "parentItem"
    in
    { name = "items"
    , from = "zotero.item i\nWHERE i.library = " ++ libraryId
    , columns = dedupe (standardColumns ++ List.map (baseColumn input.schema) bases ++ List.map fieldColumn extras)
    , retired = retiredItemField
    }


baseColumn : Schema -> String -> Column
baseColumn schema base =
    if Dict.member base typedFields then
        fieldColumn base

    else
        let
            coalesced : String
            coalesced =
                case Schema.fieldNamesForBase schema base of
                    [ single ] ->
                        "i.data->>" ++ Sql.literal single

                    names ->
                        "COALESCE(" ++ String.join ", " (List.map (\n -> "i.data->>" ++ Sql.literal n) names) ++ ")"

            value : String
            value =
                if base == "title" then
                    -- zot-sync 0.3 stored a note's HTML in items.title
                    "CASE WHEN i.item_type = 'note' THEN i.data->>'note' ELSE " ++ coalesced ++ " END"

                else
                    coalesced
        in
        { name = base, sqlType = Text, expression = "NULLIF(" ++ value ++ ", '')" }


{-| Columns every item view starts with, in zot-sync 0.3's order.
-}
standardColumns : List Column
standardColumns =
    [ Column "key" Text "i.key"
    , Column "version" Integer "i.version"
    , Column "numChildren" Integer "zotero.try_integer(i.meta->>'numChildren')"
    , Column "dateAdded" TimestampTz "zotero.try_timestamptz(i.data->>'dateAdded')"
    , Column "dateModified" TimestampTz "zotero.try_timestamptz(i.data->>'dateModified')"
    , Column "createdByUser" Jsonb "i.meta->'createdByUser'"
    , Column "lastModifiedByUser" Jsonb "i.meta->'lastModifiedByUser'"
    , Column "parsedDate" Text "i.meta->>'parsedDate'"
    , Column "creatorSummary" Text "i.meta->>'creatorSummary'"
    , Column "itemType" Text "i.item_type"
    , Column "deleted" Boolean "i.deleted"
    , Column "creators" Jsonb "i.data->'creators'"
    , Column "tags" Jsonb "i.data->'tags'"
    , Column "collections" Jsonb "i.data->'collections'"
    , Column "relations" Jsonb "i.data->'relations'"
    ]


{-| Keys of an item's `data` that the standard columns already expose.
-}
envelopeKeys : Set String
envelopeKeys =
    Set.fromList [ "key", "version", "itemType", "parentItem", "deleted", "creators", "tags", "collections", "relations", "dateAdded", "dateModified" ]


childTypes : List String
childTypes =
    [ "note", "attachment", "annotation" ]


{-| Fields the API sends for these types that the schema does not list.
-}
knownExtras : String -> List String
knownExtras itemType =
    case itemType of
        "note" ->
            [ "note", "customJSON" ]

        "attachment" ->
            [ "linkMode", "contentType", "charset", "filename", "md5", "mtime", "path", "note" ]

        "annotation" ->
            [ "annotationType", "annotationAuthorName", "annotationText", "annotationComment", "annotationColor", "annotationPageLabel", "annotationSortIndex", "annotationPosition" ]

        _ ->
            []


{-| Fields with a non-text column type. Fixed by name so it never changes.
-}
typedFields : Dict String ( SqlType, String )
typedFields =
    Dict.fromList
        [ ( "accessDate", ( Timestamp, "zotero.try_timestamp(i.data->>'accessDate')" ) )
        , ( "mtime", ( BigInt, "zotero.try_bigint(i.data->>'mtime')" ) )
        , ( "annotationPosition", ( Jsonb, "zotero.try_jsonb(i.data->>'annotationPosition')" ) )
        , ( "inPublications", ( Boolean, "zotero.try_boolean(i.data->>'inPublications')" ) )
        , ( "customJSON", ( Jsonb, "zotero.custom_json(i.data->>'note')" ) )
        , ( "parentItem", ( Text, "i.parent_key" ) )
        ]


fieldColumn : String -> Column
fieldColumn name =
    case Dict.get name typedFields of
        Just ( sqlType, expression ) ->
            Column name sqlType expression

        Nothing ->
            Column name Text ("NULLIF(i.data->>" ++ Sql.literal name ++ ", '')")


retiredItemField : String -> SqlType -> String
retiredItemField name sqlType =
    case sqlType of
        Text ->
            "NULLIF(i.data->>" ++ Sql.literal name ++ ", '')"

        Jsonb ->
            "i.data->" ++ Sql.literal name

        _ ->
            "NULL"


{-| Postgres truncates identifiers to 63 bytes; truncate first so the
catalog and the planner always agree on names. Then drop duplicates.
-}
dedupe : List Column -> List Column
dedupe columns =
    columns
        |> List.map (\c -> { c | name = String.left 63 c.name })
        |> List.foldl
            (\c ( seen, acc ) ->
                if Set.member c.name seen then
                    ( seen, acc )

                else
                    ( Set.insert c.name seen, c :: acc )
            )
            ( Set.empty, [] )
        |> Tuple.second
        |> List.reverse



-- COLLECTIONS, SEARCHES, TAGS, SETTINGS, FULL TEXT


auxiliaryNames : List String
auxiliaryNames =
    [ "items", "collections", "collectionItems", "searches", "tags", "settings", "fulltext" ]


auxiliarySpecs : String -> List Spec
auxiliarySpecs libraryId =
    let
        nullOf : String -> SqlType -> String
        nullOf _ _ =
            "NULL"

        arrayOf : String -> String
        arrayOf expr =
            "CASE WHEN jsonb_typeof(" ++ expr ++ ") = 'array' THEN " ++ expr ++ " ELSE '[]'::jsonb END"
    in
    [ { name = "collections"
      , from = "zotero.collection c\nWHERE c.library = " ++ libraryId
      , columns =
            [ Column "key" Text "c.key"
            , Column "version" Integer "c.version"
            , Column "name" Text "c.data->>'name'"
            , Column "parentCollection" Text "CASE WHEN jsonb_typeof(c.data->'parentCollection') = 'string' THEN c.data->>'parentCollection' END"
            , Column "relations" Jsonb "c.data->'relations'"
            , Column "deleted" Boolean "c.deleted"
            , Column "numCollections" Integer "zotero.try_integer(c.meta->>'numCollections')"
            , Column "numItems" Integer "zotero.try_integer(c.meta->>'numItems')"
            ]
      , retired = nullOf
      }
    , { name = "collectionItems"
      , from = "zotero.item i\nCROSS JOIN LATERAL jsonb_array_elements_text(" ++ arrayOf "i.data->'collections'" ++ ") AS ck(value)\nWHERE i.library = " ++ libraryId
      , columns = [ Column "collectionKey" Text "ck.value", Column "itemKey" Text "i.key" ]
      , retired = nullOf
      }
    , { name = "searches"
      , from = "zotero.search s\nWHERE s.library = " ++ libraryId
      , columns =
            [ Column "key" Text "s.key"
            , Column "version" Integer "s.version"
            , Column "name" Text "s.data->>'name'"
            , Column "conditions" Jsonb "s.data->'conditions'"
            , Column "deleted" Boolean "s.deleted"
            ]
      , retired = nullOf
      }
    , { name = "tags"
      , from = "zotero.item i\nCROSS JOIN LATERAL jsonb_array_elements(" ++ arrayOf "i.data->'tags'" ++ ") AS t(value)\nWHERE i.library = " ++ libraryId
      , columns =
            [ Column "itemKey" Text "i.key"
            , Column "tag" Text "t.value->>'tag'"
            , Column "type" Integer "COALESCE(zotero.try_integer(t.value->>'type'), 0)"
            ]
      , retired = nullOf
      }
    , { name = "settings"
      , from = "zotero.setting st\nWHERE st.library = " ++ libraryId
      , columns = [ Column "name" Text "st.name", Column "version" Integer "st.version", Column "value" Jsonb "st.value" ]
      , retired = nullOf
      }
    , { name = "fulltext"
      , from = "zotero.fulltext f\nWHERE f.library = " ++ libraryId
      , columns =
            [ Column "itemKey" Text "f.item_key"
            , Column "version" Integer "f.version"
            , Column "content" Text "f.data->>'content'"
            , Column "indexedChars" Integer "zotero.try_integer(f.data->>'indexedChars')"
            , Column "totalChars" Integer "zotero.try_integer(f.data->>'totalChars')"
            , Column "indexedPages" Integer "zotero.try_integer(f.data->>'indexedPages')"
            , Column "totalPages" Integer "zotero.try_integer(f.data->>'totalPages')"
            ]
      , retired = nullOf
      }
    ]



-- CATALOG


{-| Current columns of every view in the library's schema, in order.
-}
existingColumnsQuery : Library -> Statement
existingColumnsQuery library =
    Sql.statement """SELECT c.relname AS view, a.attname AS column, format_type(a.atttypid, a.atttypmod) AS type
FROM pg_catalog.pg_class c
JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
JOIN pg_catalog.pg_attribute a ON a.attrelid = c.oid
WHERE n.nspname = $1 AND c.relkind = 'v' AND a.attnum > 0 AND NOT a.attisdropped
ORDER BY c.relname, a.attnum"""
        [ Encode.string (Library.schemaName library) ]


{-| -}
existingColumnsDecoder : Decoder ( String, ExistingColumn )
existingColumnsDecoder =
    Decode.map3 (\view column sqlType -> ( view, { name = column, sqlType = parseType sqlType } ))
        (Decode.field "view" Decode.string)
        (Decode.field "column" Decode.string)
        (Decode.field "type" Decode.string)


{-| Group catalog rows by view, keeping column order.
-}
groupExisting : List ( String, ExistingColumn ) -> Dict String (List ExistingColumn)
groupExisting rows =
    rows
        |> List.foldr
            (\( view, column ) acc -> Dict.update view (\cs -> Just (column :: Maybe.withDefault [] cs)) acc)
            Dict.empty


{-| -}
observedFieldsQuery : Library -> Statement
observedFieldsQuery library =
    Sql.statement "SELECT item_type, field FROM zotero.item_field WHERE library = $1 ORDER BY item_type, field"
        [ Encode.string (Library.id library) ]


{-| -}
observedFieldsDecoder : Decoder ( String, String )
observedFieldsDecoder =
    Decode.map2 Tuple.pair (Decode.field "item_type" Decode.string) (Decode.field "field" Decode.string)
