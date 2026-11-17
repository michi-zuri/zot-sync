module Db.Layout exposing
    ( setup, layoutVersion
    , inspect, Inspection, inspectionDecoder, legacyMessage
    , inputValidationSince
    )

{-| The tables zot-sync writes to. They hold every object exactly as the Zotero
API returned it (`data`/`meta` as jsonb), so nothing is ever lost and the query
views (see `Db.Views`) can be rebuilt from them at any time.

    zotero.library      one row per synced library, with its synced version
    zotero.item         items, including child notes, attachments, annotations
    zotero.item_field   field names seen per item type (feeds the views)
    zotero.collection   collections
    zotero.search       saved searches
    zotero.setting      synced settings (tag colours, ...)
    zotero.fulltext     full-text content of attachments
    zotero.schema       the Zotero schema last fetched, with its ETag
    zotero.sync_run     one row per library sync attempt
    logs.zot_fetch      view with zot-sync 0.3's sync log columns

@docs setup, layoutVersion
@docs inspect, Inspection, inspectionDecoder, legacyMessage
@docs inputValidationSince

-}

import Db.Sql as Sql exposing (Statement)
import Json.Decode as Decode exposing (Decoder)


{-| Bumped when the table layout changes; stored in `zotero.meta`.
-}
layoutVersion : Int
layoutVersion =
    1


{-| Idempotent DDL, run in one transaction at the start of every run. The
safe casts depend on the server version (`server_version_num`).
-}
setup : Int -> List Statement
setup serverVersion =
    [ "CREATE SCHEMA IF NOT EXISTS zotero"
    , """CREATE TABLE IF NOT EXISTS zotero.meta (
    key text PRIMARY KEY,
    value text NOT NULL
)"""
    , "INSERT INTO zotero.meta (key, value) VALUES ('layout', '" ++ String.fromInt layoutVersion ++ "') ON CONFLICT (key) DO NOTHING"
    , """CREATE TABLE IF NOT EXISTS zotero.library (
    id text PRIMARY KEY,
    type text NOT NULL CHECK (type IN ('user', 'group')),
    zotero_id bigint NOT NULL,
    name text,
    version integer NOT NULL DEFAULT 0,
    fulltext_version integer NOT NULL DEFAULT 0,
    group_data jsonb,
    views_revision text,
    synced_at timestamptz
)"""
    , "ALTER TABLE zotero.library ADD COLUMN IF NOT EXISTS fulltext_version integer NOT NULL DEFAULT 0"
    , """CREATE TABLE IF NOT EXISTS zotero.item (
    library text NOT NULL REFERENCES zotero.library (id) ON DELETE CASCADE,
    key text NOT NULL,
    version integer NOT NULL,
    item_type text NOT NULL,
    parent_key text,
    deleted boolean NOT NULL DEFAULT false,
    data jsonb NOT NULL,
    meta jsonb NOT NULL DEFAULT '{}',
    synced_at timestamptz NOT NULL DEFAULT now(),
    PRIMARY KEY (library, key)
)"""
    , "CREATE INDEX IF NOT EXISTS item_library_type_idx ON zotero.item (library, item_type)"
    , "CREATE INDEX IF NOT EXISTS item_library_parent_idx ON zotero.item (library, parent_key) WHERE parent_key IS NOT NULL"
    , """CREATE TABLE IF NOT EXISTS zotero.item_field (
    library text NOT NULL REFERENCES zotero.library (id) ON DELETE CASCADE,
    item_type text NOT NULL,
    field text NOT NULL,
    PRIMARY KEY (library, item_type, field)
)"""
    , objectTable "collection"
    , objectTable "search"
    , """CREATE TABLE IF NOT EXISTS zotero.setting (
    library text NOT NULL REFERENCES zotero.library (id) ON DELETE CASCADE,
    name text NOT NULL,
    version integer NOT NULL,
    value jsonb,
    synced_at timestamptz NOT NULL DEFAULT now(),
    PRIMARY KEY (library, name)
)"""
    , """CREATE TABLE IF NOT EXISTS zotero.fulltext (
    library text NOT NULL REFERENCES zotero.library (id) ON DELETE CASCADE,
    item_key text NOT NULL,
    version integer NOT NULL,
    data jsonb NOT NULL,
    synced_at timestamptz NOT NULL DEFAULT now(),
    PRIMARY KEY (library, item_key)
)"""
    , """CREATE TABLE IF NOT EXISTS zotero.schema (
    id boolean PRIMARY KEY DEFAULT true CHECK (id),
    version integer NOT NULL,
    etag text,
    document jsonb NOT NULL,
    fetched_at timestamptz NOT NULL DEFAULT now()
)"""
    , """CREATE TABLE IF NOT EXISTS zotero.sync_run (
    id serial PRIMARY KEY,
    library text NOT NULL REFERENCES zotero.library (id) ON DELETE CASCADE,
    started_at timestamptz NOT NULL DEFAULT now(),
    finished_at timestamptz,
    from_version integer,
    to_version integer,
    status text NOT NULL DEFAULT 'running' CHECK (status IN ('running', 'ok', 'failed')),
    error text,
    stats jsonb
)"""
    , safeCast serverVersion "try_jsonb" "jsonb"
    , safeCast serverVersion "try_integer" "integer"
    , safeCast serverVersion "try_bigint" "bigint"
    , safeCast serverVersion "try_boolean" "boolean"
    , safeCast serverVersion "try_timestamp" "timestamp"
    , safeCast serverVersion "try_timestamptz" "timestamptz"
    , -- zot-sync 0.3's customJSON: the first {...} on a single line of a note, parsed as JSON.
      -- (?n) makes '.' stop at newlines, like Python's re.findall(r'{.*}', note)[0].
      -- A note without JSON, or with JSON that does not parse, gives NULL instead of an error.
      -- Labelled like try_jsonb, so that both are inlined into the queries that use them.
      "CREATE OR REPLACE FUNCTION zotero.custom_json(note text) RETURNS jsonb\nLANGUAGE sql STABLE "
        ++ parallelLabel serverVersion
        ++ " AS\n$$ SELECT zotero.try_jsonb(substring(note from '(?n)\\{.*\\}')) $$"
    , "CREATE SCHEMA IF NOT EXISTS logs"
    , """CREATE OR REPLACE VIEW logs.zot_fetch AS
SELECT r.id,
       r.started_at AS "timestamp",
       r.to_version AS version,
       'zot_' || left(l.type, 1) || '_' || l.zotero_id AS library,
       COALESCE(r.error, l.name) AS name,
       CASE WHEN r.status = 'ok'
            THEN ceil(extract(epoch FROM r.finished_at - r.started_at))::integer
       END AS duration
FROM zotero.sync_run r
JOIN zotero.library l ON l.id = r.library"""
    ]
        |> List.map (\sql -> Sql.statement sql [])


objectTable : String -> String
objectTable name =
    "CREATE TABLE IF NOT EXISTS zotero." ++ name ++ """ (
    library text NOT NULL REFERENCES zotero.library (id) ON DELETE CASCADE,
    key text NOT NULL,
    version integer NOT NULL,
    deleted boolean NOT NULL DEFAULT false,
    data jsonb NOT NULL,
    meta jsonb NOT NULL DEFAULT '{}',
    synced_at timestamptz NOT NULL DEFAULT now(),
    PRIMARY KEY (library, key)
)"""


{-| The first `server_version_num` with `pg_input_is_valid()` (PostgreSQL 16).
-}
inputValidationSince : Int
inputValidationSince =
    160000


{-| A cast that returns NULL instead of raising, so one odd value (an
`accessDate` of "CURRENT\_TIMESTAMP", a note with broken JSON) can never make a
query over a view fail.

From PostgreSQL 16, `pg_input_is_valid()` tests the input first. That needs no
exception handler, so no subtransaction, and only invalid input becomes NULL:
any other error is raised. A PL/pgSQL `EXCEPTION WHEN others` handler also
swallowed unrelated errors, which on PostgreSQL 18 silently turned valid values
into NULL (michi-zuri/zot-sync#1). Older servers keep that handler, as they
have no other way to test a cast.

The cast is applied to the result of the CASE, not inside it: the function is
inlined, and with a constant argument Postgres would otherwise fold
`'{broken'::jsonb` while planning and raise before the CASE could skip it.

All casts are STABLE: `pg_input_is_valid()` is, and an SQL function is only
inlined when its label is no stricter than its body. No index uses them. The
exception handler's subtransaction cannot run in a parallel worker, so the
fallback is PARALLEL UNSAFE (0.4.0 wrongly said SAFE, which fails parallel
queries).

-}
safeCast : Int -> String -> String -> String
safeCast serverVersion name sqlType =
    let
        signature : String
        signature =
            "CREATE OR REPLACE FUNCTION zotero." ++ name ++ "(value text) RETURNS " ++ sqlType
    in
    if serverVersion >= inputValidationSince then
        signature
            ++ "\nLANGUAGE sql STABLE PARALLEL SAFE AS\n$$ SELECT (CASE WHEN pg_input_is_valid(value, "
            ++ Sql.literal sqlType
            ++ ") THEN value END)::"
            ++ sqlType
            ++ " $$"

    else
        signature
            ++ "\nLANGUAGE plpgsql STABLE PARALLEL UNSAFE AS\n$$ BEGIN RETURN value::"
            ++ sqlType
            ++ "; EXCEPTION WHEN others THEN RETURN NULL; END $$"


{-| Functions built on the exception-handler casts must not run in parallel workers.
-}
parallelLabel : Int -> String
parallelLabel serverVersion =
    if serverVersion >= inputValidationSince then
        "PARALLEL SAFE"

    else
        "PARALLEL UNSAFE"


{-| What `setup` needs to know about the database first.
-}
type alias Inspection =
    { serverVersion : Int
    , legacyTables : List String
    }


{-| The server version, and the tables from zot-sync 0.3 whose names the new
views reuse (a run refuses to start if there are any). One row.
-}
inspect : Statement
inspect =
    Sql.statement """SELECT current_setting('server_version_num')::integer AS server_version,
       ARRAY(SELECT n.nspname || '.' || c.relname
             FROM pg_catalog.pg_class c
             JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
             WHERE c.relkind IN ('r', 'p')
               AND ((n.nspname = 'logs' AND c.relname = 'zot_fetch')
                    OR (n.nspname ~ '^zot_[ug]_[0-9]+$' AND c.relname = 'items'))
             ORDER BY 1) AS legacy_tables""" []


{-| -}
inspectionDecoder : Decoder Inspection
inspectionDecoder =
    Decode.map2 Inspection
        (Decode.field "server_version" Decode.int)
        (Decode.field "legacy_tables" (Decode.list Decode.string))


{-| -}
legacyMessage : List String -> String
legacyMessage tables =
    "This database still has the zot-sync 0.3 layout ("
        ++ String.join ", " tables
        ++ " are tables).\nzot-sync 0.4 starts from a fresh mirror: point --db at a new database, "
        ++ "or drop the logs and zot_* schemas first. Your queries against the old view and table names keep working on the new mirror."
