# zot-sync

Mirror Zotero libraries into PostgreSQL, so you can query them with SQL.

zot-sync is a one-way sync from the [Zotero Web API](https://www.zotero.org/support/dev/web_api/v3/start)
into Postgres. It mirrors everything in a user or group library: items with their child notes,
attachments and annotations (trash included), collections, saved searches, tags, synced
settings and the full-text content of attachments. Changes to Zotero's item schema are picked
up automatically. It is an [elm-pages](https://elm-pages.com) script.

## Usage

Requires Node 20 or newer and PostgreSQL (tested with 16; any version from 10 should work).

```bash
git clone https://github.com/michi-zuri/zot-sync && cd zot-sync
npm ci
npm run build   # writes dist/zot-sync.mjs, a single self-contained file that only needs Node
```

```bash
node dist/zot-sync.mjs fetch --db postgres://user:pass@localhost/zotero --key YOUR_ZOTERO_KEY
```

| Command | Syncs |
| --- | --- |
| `fetch --db URL --key KEY` | every library the key can read |
| `fetch --db URL --user ID [--key KEY]` | one user library (public ones need no key) |
| `fetch --db URL --group ID [--key KEY]` | one group library |
| `fetch --db URL --user ID --all` or `--key KEY --all` | a user's library and all their groups |
| `check --db URL` / `check --key KEY` | checks the database, or shows what a key can read |

More options for `fetch`: `--skip` leaves out the user library (with `--all` or `--key`),
`--skip-fulltext` skips attachment full text, `--verbose` logs every request. API keys are
created at <https://www.zotero.org/settings/keys>. Run zot-sync as often as you like (cron, a
timer): an unchanged library costs one small request per run.

Without building, `npx elm-pages run ZotSync fetch ...` runs the same script from source.

## What ends up in Postgres

Each library gets a schema named like zot-sync 0.3's (`zot_u_<userID>`, `zot_g_<groupID>`)
with these views:

| View | Rows |
| --- | --- |
| `items` | every item, with one column per generic Zotero field (`title`, `date`, `publicationTitle`, ...) |
| `"journalArticle"`, `book`, `note`, `attachment`, `annotation`, ... | one view per item type, with that type's own field names (`caseName`, `court`, ...) |
| `collections`, `"collectionItems"` | collections (with `"parentCollection"`) and which items they contain |
| `searches`, `tags`, `settings`, `fulltext` | saved searches, item tags, synced settings, attachment full text |

Column names are Zotero's camelCase field names, so quote them: `SELECT "publicationTitle" FROM
zot_u_123."journalArticle"`. Child items have a `"parentItem"` column, trashed objects
`deleted = true`. The `note` view (and `items`) has `"customJSON"`: the first `{...}` on a line
of the note, parsed as JSON. A note without such JSON, or with JSON that does not parse,
gives `NULL` there; it never fails a sync or a query.

The views read from tables in the `zotero` schema, which hold every object exactly as the API
returned it (`data` and `meta` as `jsonb`), so nothing is lost even for fields no view knows
about yet:

    zotero.library      synced libraries and their versions
    zotero.item         items, including child items and the trash
    zotero.collection   zotero.search   zotero.setting   zotero.fulltext
    zotero.sync_run     one row per library sync, with status, error and counts
    zotero.schema       the Zotero schema last fetched

`logs.zot_fetch` keeps zot-sync 0.3's sync log columns as a view over `zotero.sync_run`.

### When Zotero's schema changes

Zotero regularly adds fields to item types, usually in the middle of a type's field list
(127 times between 2021 and 2026, never at the end). zot-sync fetches the schema on every run
and regenerates the views from it. A view's existing columns always keep their name, type
and position, and new fields are appended, so the regeneration never fails and views you
built on top of these keep working. A field Zotero removes stays as a column. Fields that
appear in the data before the schema lists them (as with notes, attachments and annotations,
whose fields the schema does not list at all) get columns too.

### Coming from zot-sync 0.3

zot-sync 0.4 starts with a fresh mirror: point `--db` at a new database, or drop the `logs`
and `zot_*` schemas. It refuses to run against a 0.3 database. The schema, view and column
names are the same, so most queries keep working. The differences:

- `items` and the `note` and `attachment` views now include child items; add
  `WHERE "parentItem" IS NULL` for 0.3's top-level-only behaviour.
- `items` is a view now; columns are `text` rather than `varchar(65535)`.
- 0.3 stopped adopting schema changes once Zotero inserted a field mid-list; it also lost
  item type changes and kept restored items marked as deleted. Those are fixed.

## How a sync works

It follows the official Zotero client's sync protocol. For each library: settings are fetched
with `If-Modified-Since-Version` (a 304 ends the run for that library); then collections,
searches and items are listed by version and the changed ones fetched by key, 100 per request;
then deletions are applied, then full text. If the library changes during a sync, it starts
over; if the server's delete log no longer reaches back to the last sync, every object is
reconciled. The library's version is recorded only once everything, views included, has been
written, so an interrupted run is simply repeated next time. `Backoff` and `Retry-After`
from the API are honoured. If one library fails, it is recorded in `zotero.sync_run` and
the others are still synced.

## Development

```bash
npm test                  # Elm unit tests, incl. whole runs against simulated APIs
npm run test:integration  # the real script against Postgres and a fake Zotero API
```

The integration tests need a database they may wipe: `ZOT_SYNC_TEST_DB` (default
`postgresql://root@/zot_sync_test?host=/var/run/postgresql`).

The code is in `script/src`: `ZotSync.elm` (command line), `Sync.elm` (the sync), `Zotero/*`
(API requests and decoders), `Db/*` (tables, views, statements). `script/custom-backend-task.ts`
is the only JavaScript: it runs the SQL the Elm code builds.
