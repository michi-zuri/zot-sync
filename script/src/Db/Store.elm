module Db.Store exposing
    ( Table(..)
    , ensureLibrary, libraryState, LibraryState, libraryStateDecoder
    , startRun, runIdDecoder, finishRun, failRun, finishLibrary
    , upsertItems, recordItemFields, upsertEntities, upsertSettings, upsertFulltext
    , deleteKeys, deleteKeysExcept, localVersions, localVersionDecoder
    , cachedSchema, CachedSchema, cachedSchemaDecoder, storeSchema
    )

{-| Statements that read and write the mirror (`Db.Layout`). Batches travel
as one JSON parameter expanded with `jsonb_to_recordset`, so a batch of 100
objects is one statement. Upserts never replace a newer version with an older
one, so retries and overlapping runs are harmless.

@docs Table
@docs ensureLibrary, libraryState, LibraryState, libraryStateDecoder
@docs startRun, runIdDecoder, finishRun, failRun, finishLibrary
@docs upsertItems, recordItemFields, upsertEntities, upsertSettings, upsertFulltext
@docs deleteKeys, deleteKeysExcept, localVersions, localVersionDecoder
@docs cachedSchema, CachedSchema, cachedSchemaDecoder, storeSchema

-}

import Db.Sql as Sql exposing (Statement)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Set
import Zotero.Library as Library exposing (Library)
import Zotero.Object exposing (Entity, Item, Setting)


{-| -}
type Table
    = ItemTable
    | CollectionTable
    | SearchTable
    | SettingTable
    | FulltextTable


tableName : Table -> String
tableName table =
    case table of
        ItemTable ->
            "zotero.item"

        CollectionTable ->
            "zotero.collection"

        SearchTable ->
            "zotero.search"

        SettingTable ->
            "zotero.setting"

        FulltextTable ->
            "zotero.fulltext"


keyColumn : Table -> String
keyColumn table =
    case table of
        SettingTable ->
            "name"

        FulltextTable ->
            "item_key"

        _ ->
            "key"


libraryParam : Library -> Encode.Value
libraryParam library =
    Encode.string (Library.id library)


{-| jsonb parameters travel as text: node-postgres would turn a JS array into
a Postgres array literal.
-}
jsonParam : Encode.Value -> Encode.Value
jsonParam value =
    Encode.string (Encode.encode 0 value)


keysParam : List String -> Encode.Value
keysParam keys =
    jsonParam (Encode.list Encode.string keys)



-- LIBRARY AND RUN BOOKKEEPING


{-| -}
ensureLibrary : Library -> Statement
ensureLibrary library =
    Sql.statement "INSERT INTO zotero.library (id, type, zotero_id) VALUES ($1, $2, $3) ON CONFLICT (id) DO NOTHING"
        [ libraryParam library, Encode.string (Library.typeName library), Encode.int (Library.zoteroId library) ]


{-| -}
libraryState : Library -> Statement
libraryState library =
    Sql.statement "SELECT version, views_revision, name FROM zotero.library WHERE id = $1" [ libraryParam library ]


{-| -}
type alias LibraryState =
    { version : Int
    , viewsRevision : Maybe String
    , name : Maybe String
    }


{-| -}
libraryStateDecoder : Decoder LibraryState
libraryStateDecoder =
    Decode.map3 LibraryState
        (Decode.field "version" Decode.int)
        (Decode.field "views_revision" (Decode.nullable Decode.string))
        (Decode.oneOf [ Decode.field "name" (Decode.nullable Decode.string), Decode.succeed Nothing ])


{-| -}
startRun : Library -> Int -> Statement
startRun library fromVersion =
    Sql.statement "INSERT INTO zotero.sync_run (library, from_version) VALUES ($1, $2) RETURNING id"
        [ libraryParam library, Encode.int fromVersion ]


{-| -}
runIdDecoder : Decoder Int
runIdDecoder =
    Decode.field "id" Decode.int


{-| -}
finishRun : Int -> Int -> Encode.Value -> Statement
finishRun runId toVersion stats =
    Sql.statement "UPDATE zotero.sync_run SET finished_at = now(), status = 'ok', to_version = $2, stats = $3::jsonb WHERE id = $1"
        [ Encode.int runId, Encode.int toVersion, jsonParam stats ]


{-| -}
failRun : Int -> String -> Statement
failRun runId message =
    Sql.statement "UPDATE zotero.sync_run SET finished_at = now(), status = 'failed', error = $2 WHERE id = $1"
        [ Encode.int runId, Encode.string message ]


{-| Record the synced version. Only moves forward, so an older concurrent run
can never roll the library back.
-}
finishLibrary : Library -> { version : Int, name : Maybe String, groupData : Maybe Encode.Value, viewsRevision : Maybe String } -> Statement
finishLibrary library state =
    Sql.statement """UPDATE zotero.library
SET version = GREATEST(version, $2),
    name = COALESCE($3, name),
    group_data = COALESCE($4::jsonb, group_data),
    views_revision = COALESCE($5, views_revision),
    synced_at = now()
WHERE id = $1"""
        [ libraryParam library
        , Encode.int state.version
        , state.name |> Maybe.map Encode.string |> Maybe.withDefault Encode.null
        , state.groupData |> Maybe.map jsonParam |> Maybe.withDefault Encode.null
        , state.viewsRevision |> Maybe.map Encode.string |> Maybe.withDefault Encode.null
        ]



-- WRITES


{-| -}
upsertItems : Library -> List Item -> Statement
upsertItems library items =
    Sql.statement """INSERT INTO zotero.item AS stored (library, key, version, item_type, parent_key, deleted, data, meta)
SELECT $1, x.key, x.version, x.item_type, x.parent_key, x.deleted, x.data, x.meta
FROM jsonb_to_recordset($2::jsonb) AS x(key text, version integer, item_type text, parent_key text, deleted boolean, data jsonb, meta jsonb)
ON CONFLICT (library, key) DO UPDATE
SET version = EXCLUDED.version, item_type = EXCLUDED.item_type, parent_key = EXCLUDED.parent_key,
    deleted = EXCLUDED.deleted, data = EXCLUDED.data, meta = EXCLUDED.meta, synced_at = now()
WHERE stored.version <= EXCLUDED.version"""
        [ libraryParam library
        , items
            |> Encode.list
                (\item ->
                    Encode.object
                        [ ( "key", Encode.string item.key )
                        , ( "version", Encode.int item.version )
                        , ( "item_type", Encode.string item.itemType )
                        , ( "parent_key", item.parentKey |> Maybe.map Encode.string |> Maybe.withDefault Encode.null )
                        , ( "deleted", Encode.bool item.deleted )
                        , ( "data", item.data )
                        , ( "meta", item.meta )
                        ]
                )
            |> jsonParam
        ]


{-| Remember which fields each item type actually carries, so the views can
offer them as columns even when the schema does not list them.
-}
recordItemFields : Library -> List ( String, String ) -> Statement
recordItemFields library pairs =
    Sql.statement """INSERT INTO zotero.item_field (library, item_type, field)
SELECT DISTINCT $1, x.item_type, x.field
FROM jsonb_to_recordset($2::jsonb) AS x(item_type text, field text)
ON CONFLICT DO NOTHING"""
        [ libraryParam library
        , pairs
            |> Set.fromList
            |> Set.toList
            |> Encode.list (\( itemType, field ) -> Encode.object [ ( "item_type", Encode.string itemType ), ( "field", Encode.string field ) ])
            |> jsonParam
        ]


{-| Collections or saved searches.
-}
upsertEntities : Table -> Library -> List Entity -> Statement
upsertEntities table library entities =
    Sql.statement
        ("INSERT INTO "
            ++ tableName table
            ++ """ AS stored (library, key, version, deleted, data, meta)
SELECT $1, x.key, x.version, x.deleted, x.data, x.meta
FROM jsonb_to_recordset($2::jsonb) AS x(key text, version integer, deleted boolean, data jsonb, meta jsonb)
ON CONFLICT (library, key) DO UPDATE
SET version = EXCLUDED.version, deleted = EXCLUDED.deleted, data = EXCLUDED.data, meta = EXCLUDED.meta, synced_at = now()
WHERE stored.version <= EXCLUDED.version"""
        )
        [ libraryParam library
        , entities
            |> Encode.list
                (\e ->
                    Encode.object
                        [ ( "key", Encode.string e.key )
                        , ( "version", Encode.int e.version )
                        , ( "deleted", Encode.bool e.deleted )
                        , ( "data", e.data )
                        , ( "meta", e.meta )
                        ]
                )
            |> jsonParam
        ]


{-| -}
upsertSettings : Library -> List Setting -> Statement
upsertSettings library settings =
    Sql.statement """INSERT INTO zotero.setting AS stored (library, name, version, value)
SELECT $1, x.name, x.version, x.value
FROM jsonb_to_recordset($2::jsonb) AS x(name text, version integer, value jsonb)
ON CONFLICT (library, name) DO UPDATE
SET version = EXCLUDED.version, value = EXCLUDED.value, synced_at = now()
WHERE stored.version <= EXCLUDED.version"""
        [ libraryParam library
        , settings
            |> Encode.list (\s -> Encode.object [ ( "name", Encode.string s.name ), ( "version", Encode.int s.version ), ( "value", s.value ) ])
            |> jsonParam
        ]


{-| -}
upsertFulltext : Library -> List { key : String, version : Int, data : Encode.Value } -> Statement
upsertFulltext library entries =
    Sql.statement """INSERT INTO zotero.fulltext AS stored (library, item_key, version, data)
SELECT $1, x.item_key, x.version, x.data
FROM jsonb_to_recordset($2::jsonb) AS x(item_key text, version integer, data jsonb)
ON CONFLICT (library, item_key) DO UPDATE
SET version = EXCLUDED.version, data = EXCLUDED.data, synced_at = now()
WHERE stored.version <= EXCLUDED.version"""
        [ libraryParam library
        , entries
            |> Encode.list (\e -> Encode.object [ ( "item_key", Encode.string e.key ), ( "version", Encode.int e.version ), ( "data", e.data ) ])
            |> jsonParam
        ]


{-| -}
deleteKeys : Table -> Library -> List String -> Statement
deleteKeys table library keys =
    Sql.statement
        ("DELETE FROM " ++ tableName table ++ " WHERE library = $1 AND " ++ keyColumn table ++ " IN (SELECT jsonb_array_elements_text($2::jsonb))")
        [ libraryParam library, keysParam keys ]


{-| For a full sync: remove everything the server no longer has.
-}
deleteKeysExcept : Table -> Library -> List String -> Statement
deleteKeysExcept table library keys =
    Sql.statement
        ("DELETE FROM " ++ tableName table ++ " WHERE library = $1 AND " ++ keyColumn table ++ " NOT IN (SELECT jsonb_array_elements_text($2::jsonb))")
        [ libraryParam library, keysParam keys ]


{-| Local versions, of all objects (`Nothing`) or of the given keys.
-}
localVersions : Table -> Library -> Maybe (List String) -> Statement
localVersions table library keys =
    let
        select : String
        select =
            "SELECT " ++ keyColumn table ++ " AS key, version FROM " ++ tableName table ++ " WHERE library = $1"
    in
    case keys of
        Nothing ->
            Sql.statement select [ libraryParam library ]

        Just someKeys ->
            Sql.statement (select ++ " AND " ++ keyColumn table ++ " IN (SELECT jsonb_array_elements_text($2::jsonb))")
                [ libraryParam library, keysParam someKeys ]


{-| -}
localVersionDecoder : Decoder ( String, Int )
localVersionDecoder =
    Decode.map2 Tuple.pair (Decode.field "key" Decode.string) (Decode.field "version" Decode.int)



-- SCHEMA CACHE


{-| -}
type alias CachedSchema =
    { version : Int
    , etag : Maybe String
    , document : Decode.Value
    }


{-| -}
cachedSchema : Statement
cachedSchema =
    Sql.statement "SELECT version, etag, document FROM zotero.schema" []


{-| -}
cachedSchemaDecoder : Decoder CachedSchema
cachedSchemaDecoder =
    Decode.map3 CachedSchema
        (Decode.field "version" Decode.int)
        (Decode.field "etag" (Decode.nullable Decode.string))
        (Decode.field "document" Decode.value)


{-| -}
storeSchema : CachedSchema -> Statement
storeSchema schema =
    Sql.statement """INSERT INTO zotero.schema (id, version, etag, document, fetched_at)
VALUES (true, $1, $2, $3::jsonb, now())
ON CONFLICT (id) DO UPDATE SET version = EXCLUDED.version, etag = EXCLUDED.etag, document = EXCLUDED.document, fetched_at = now()"""
        [ Encode.int schema.version
        , schema.etag |> Maybe.map Encode.string |> Maybe.withDefault Encode.null
        , jsonParam schema.document
        ]
