// End-to-end: the real script (elm-pages run), a real Postgres, a fake Zotero.
//
//   npm run test:integration
//
// Needs a Postgres database the tests may freely wipe; set ZOT_SYNC_TEST_DB
// (default: postgresql://root@/zot_sync_test?host=/var/run/postgresql).
import { test, before, after, beforeEach } from "node:test";
import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import pg from "pg";
import { FakeLibrary, startFakeZotero } from "./fake-zotero.mjs";

const root = join(dirname(fileURLToPath(import.meta.url)), "..", "..");
const fixture = (name) => JSON.parse(readFileSync(join(root, "script", "tests", "fixtures", name), "utf8"));
const DB = process.env.ZOT_SYNC_TEST_DB ?? "postgresql://root@/zot_sync_test?host=/var/run/postgresql";
const USER = 1234567;
const GROUP = 2345678;
const KEY = "K";

const schemas = {
  v32: { etag: '"schema-v32"', document: fixture("schema-v32.json") },
  v42: { etag: '"schema-v42"', document: fixture("schema-v42.json") },
};

const keys = {
  [KEY]: {
    userID: USER,
    username: "zot-sync-tester",
    access: { user: { library: true, files: true, notes: true, write: false }, groups: { all: { library: true, write: false } } },
  },
};

let db;
before(async () => {
  db = new pg.Client({ connectionString: DB });
  await db.connect();
});
after(async () => db?.end());
beforeEach(async () => {
  const { rows } = await db.query(
    "SELECT nspname FROM pg_namespace WHERE nspname IN ('zotero', 'logs') OR nspname ~ '^zot_[ug]_[0-9]+$'"
  );
  for (const { nspname } of rows) await db.query(`DROP SCHEMA "${nspname}" CASCADE`);
  await db.query("DROP VIEW IF EXISTS public.my_podcasts");
});

const all = async (sql, params = []) => (await db.query(sql, params)).rows;
const one = async (sql, params = []) => (await all(sql, params))[0];

/** Run the CLI; resolves with its exit code and combined output. */
function zotSync(args) {
  return new Promise((resolve) => {
    const child = spawn(join(root, "node_modules", ".bin", "elm-pages"), ["run", "ZotSync", ...args], {
      cwd: root,
      env: { ...process.env, NO_COLOR: "1", FORCE_COLOR: "0" },
    });
    let out = "";
    child.stdout.on("data", (d) => (out += d));
    child.stderr.on("data", (d) => (out += d));
    child.on("close", (code) => resolve({ code, out }));
  });
}

function userLibrary() {
  const lib = new FakeLibrary({ type: "user", id: USER, name: "zot-sync tester" });
  for (const c of fixture("collections.json")) lib.put("collections", c.data, c.meta);
  for (const s of fixture("searches.json")) lib.put("searches", s.data, s.meta ?? {});
  for (const [name, s] of Object.entries(fixture("settings.json"))) lib.setSetting(name, s.value);
  for (const i of [...fixture("items-top.json"), ...fixture("items-children.json")]) lib.put("items", i.data, i.meta);
  lib.setFulltext("HI9J2K3L", fixture("fulltext-pdf.json"));
  return lib;
}

function groupLibrary(extra = {}) {
  const lib = new FakeLibrary({
    type: "group",
    id: GROUP,
    name: "Sync Research Group",
    group: { version: 7, owner: USER, type: "Private", members: [USER], libraryReading: "members", ...extra },
  });
  const article = fixture("items-top.json")[0];
  lib.put("items", { ...article.data, key: "GR2O3U4P", collections: [] }, article.meta);
  return lib;
}

async function withZotero(libraries, fn, schema = "v42") {
  const zotero = await startFakeZotero({ libraries, keys, schemas, schema });
  try {
    return await fn(zotero);
  } finally {
    await zotero.close();
  }
}

const fetchArgs = (zotero, ...rest) => ["fetch", "--db", DB, "--api-url", zotero.url, ...rest];

test("first sync mirrors items with their children, collections, searches, settings and full text", async () => {
  const lib = userLibrary();
  await withZotero([lib], async (zotero) => {
    const run = await zotSync(fetchArgs(zotero, "--user", String(USER), "--key", KEY));
    assert.equal(run.code, 0, run.out);
    assert.match(
      run.out,
      new RegExp(`zot_u_${USER} "zot-sync tester": version 0 -> ${lib.version} · 11 items, 3 collections, 1 saved search, 2 settings, 1 full text · views updated`)
    );

    assert.deepEqual(
      await one(`SELECT count(*)::int AS items, count(*) FILTER (WHERE parent_key IS NOT NULL)::int AS children,
                        count(*) FILTER (WHERE deleted)::int AS trashed FROM zotero.item`),
      { items: 11, children: 5, trashed: 2 }
    );

    const s = `zot_u_${USER}`;
    assert.deepEqual(
      await one(`SELECT title, "publicationTitle", "DOI", "accessDate"::text AS accessed, "numChildren" FROM ${s}."journalArticle"`),
      { title: "Mirroring bibliographies", publicationTitle: "Journal of Sync", DOI: "10.1234/sync.7.2", accessed: "2021-06-01 10:00:00", numChildren: 3 }
    );
    // Zotero's literal "CURRENT_TIMESTAMP" access date reads as NULL instead of breaking the view.
    assert.deepEqual(await one(`SELECT title, "accessDate" FROM ${s}.book`), { title: "Versioned Data", accessDate: null });
    // Type-specific names in the type view, generic names in the items view, like zot-sync 0.3.
    assert.deepEqual(await one(`SELECT "caseName", court FROM ${s}."case"`), { caseName: "Doe v. Roe", court: "Supreme Court" });
    assert.deepEqual(await one(`SELECT title, authority FROM ${s}.items WHERE key = 'CD4E5F6G'`), { title: "Doe v. Roe", authority: "Supreme Court" });

    // customJSON: parsed where it parses, NULL (not an error) where it does not.
    assert.deepEqual(await all(`SELECT key, "parentItem", "customJSON" FROM ${s}.note ORDER BY key`), [
      { key: "DE5F6G7H", parentItem: null, customJSON: { decision: "include", score: 4 } },
      { key: "GH8I9J2K", parentItem: "AB2C3D4E", customJSON: null },
      { key: "KL4M5N6P", parentItem: "BC3D4E5F", customJSON: null },
    ]);
    const noteInItems = await one(`SELECT title, "customJSON" FROM ${s}.items WHERE key = 'DE5F6G7H'`);
    assert.match(noteInItems.title, /^<p>screening decision<\/p>/);
    assert.deepEqual(noteInItems.customJSON, { decision: "include", score: 4 });

    assert.deepEqual(await all(`SELECT key, "linkMode", md5, mtime::text AS mtime, "parentItem" FROM ${s}.attachment ORDER BY key`), [
      { key: "EF6G7H8I", linkMode: "linked_url", md5: null, mtime: null, parentItem: null },
      { key: "HI9J2K3L", linkMode: "imported_url", md5: "9e107d9d372bb6826bd81d3542a419d6", mtime: "1622541660000", parentItem: "AB2C3D4E" },
      { key: "IJ2K3L4M", linkMode: "imported_url", md5: null, mtime: null, parentItem: "AB2C3D4E" },
    ]);
    assert.deepEqual(
      await one(`SELECT "parentItem", "annotationText", "annotationPosition"->>'pageIndex' AS page FROM ${s}.annotation`),
      { parentItem: "HI9J2K3L", annotationText: "mirrors drift", page: "2" }
    );

    assert.deepEqual(await all(`SELECT key, name, "parentCollection", deleted, "numItems" FROM ${s}.collections ORDER BY key`), [
      { key: "CQ2R3S4T", name: "Systematic review", parentCollection: null, deleted: false, numItems: 3 },
      { key: "DR3S4T5U", name: "Included", parentCollection: "CQ2R3S4T", deleted: false, numItems: 1 },
      { key: "ES4T5U6V", name: "Old drafts", parentCollection: null, deleted: true, numItems: 0 },
    ]);
    assert.deepEqual(
      await all(`SELECT "itemKey" FROM ${s}."collectionItems" WHERE "collectionKey" = 'CQ2R3S4T' ORDER BY 1`),
      [{ itemKey: "AB2C3D4E" }, { itemKey: "BC3D4E5F" }, { itemKey: "DE5F6G7H" }]
    );
    assert.deepEqual(await all(`SELECT tag, type FROM ${s}.tags WHERE "itemKey" = 'AB2C3D4E' ORDER BY tag`), [
      { tag: "postgres", type: 1 },
      { tag: "replication", type: 0 },
    ]);
    assert.deepEqual(await one(`SELECT name, jsonb_array_length(conditions) AS n FROM ${s}.searches`), { name: "Unscreened PDFs", n: 2 });
    assert.deepEqual((await one(`SELECT value FROM ${s}.settings WHERE name = 'tagColors'`)).value, [{ name: "important", color: "#ff6666" }]);
    assert.deepEqual(await one(`SELECT content, "indexedPages" FROM ${s}.fulltext`), {
      content: "Mirrors drift when versions are skipped.",
      indexedPages: 3,
    });

    const log = await one(`SELECT library, name, version, duration IS NOT NULL AS finished FROM logs.zot_fetch`);
    assert.deepEqual(log, { library: `zot_u_${USER}`, name: "zot-sync tester", version: lib.version, finished: true });
  });
});

test("an incremental sync applies only what changed, including the cases zot-sync 0.3 got wrong", async () => {
  const lib = userLibrary();
  await withZotero([lib], async (zotero) => {
    assert.equal((await zotSync(fetchArgs(zotero, "--user", String(USER), "--key", KEY))).code, 0);
    const before = lib.version;
    const [article, book, , , , podcast] = fixture("items-top.json");

    lib.put("items", { ...article.data, title: "Mirroring bibliographies, revised" }, article.meta);
    lib.put("items", { ...book.data, itemType: "bookSection", bookTitle: "Versioned Data" }, book.meta); // 0.3 lost type changes
    const { deleted, ...restored } = podcast.data;
    lib.put("items", restored, podcast.meta); // 0.3 kept restored items marked deleted
    lib.put("items", { key: "NO2T3E4S", itemType: "note", parentItem: "CD4E5F6G", note: "<p>new child note</p>", tags: [], relations: {} });
    lib.remove("items", "EF6G7H8I");
    lib.removeSetting("lastPageIndex_u_HI9J2K3L");
    lib.remove("collections", "ES4T5U6V");
    zotero.requests.length = 0;

    const run = await zotSync(fetchArgs(zotero, "--user", String(USER), "--key", KEY));
    assert.equal(run.code, 0, run.out);
    assert.match(run.out, new RegExp(`version ${before} -> ${lib.version} · 4 items, 3 deletions · views updated`));
    const listings = zotero.requests.filter((r) => r.query.format === "versions");
    assert.ok(listings.length > 0 && listings.every((r) => Number(r.query.since) === before), "every listing asks only for changes");

    const s = `zot_u_${USER}`;
    assert.equal((await one(`SELECT title FROM ${s}."journalArticle"`)).title, "Mirroring bibliographies, revised");
    assert.deepEqual(await all(`SELECT key FROM ${s}.book`), []);
    assert.deepEqual(await one(`SELECT key, "bookTitle" FROM ${s}."bookSection"`), { key: "BC3D4E5F", bookTitle: "Versioned Data" });
    assert.equal((await one(`SELECT deleted FROM ${s}.podcast`)).deleted, false);
    assert.deepEqual(await one(`SELECT "parentItem" FROM ${s}.note WHERE key = 'NO2T3E4S'`), { parentItem: "CD4E5F6G" });
    assert.equal((await one(`SELECT count(*)::int AS n FROM zotero.item WHERE key = 'EF6G7H8I'`)).n, 0);
    assert.deepEqual(await all(`SELECT name FROM ${s}.settings`), [{ name: "tagColors" }]);
    assert.equal((await one(`SELECT count(*)::int AS n FROM ${s}.collections`)).n, 2);
  });
});

test("an unchanged library costs two requests and stays put", async () => {
  const lib = userLibrary();
  await withZotero([lib], async (zotero) => {
    assert.equal((await zotSync(fetchArgs(zotero, "--user", String(USER), "--key", KEY))).code, 0);
    zotero.requests.length = 0;
    const run = await zotSync(fetchArgs(zotero, "--user", String(USER), "--key", KEY));
    assert.equal(run.code, 0, run.out);
    assert.match(run.out, new RegExp(`zot_u_${USER} "zot-sync tester": up to date at version ${lib.version}\\n`));
    assert.deepEqual(
      zotero.requests.map((r) => r.path),
      ["/schema", `/users/${USER}/settings`]
    );
  });
});

test("a Zotero schema change is adopted on the next run, and views built on top keep working", async () => {
  // While the API serves schema v32, items carry only v32's fields.
  const v32Podcast = schemas.v32.document.itemTypes.find((t) => t.itemType === "podcast").fields.map((f) => f.field);
  const lib = new FakeLibrary({ type: "user", id: USER, name: "zot-sync tester" });
  for (const [key, title] of [["PO2D3C4S", "Sync Talk"], ["PO3D4C5S", "Sync Talk 2"]]) {
    const data = { key, itemType: "podcast", creators: [], tags: [], collections: [], relations: {} };
    for (const field of v32Podcast) data[field] = field === "title" ? title : "";
    lib.put("items", data, { numChildren: 0 });
  }

  await withZotero(
    [lib],
    async (zotero) => {
      assert.equal((await zotSync(fetchArgs(zotero, "--user", String(USER), "--key", KEY))).code, 0);
      const s = `zot_u_${USER}`;
      const columns = async () =>
        (await all(`SELECT attname FROM pg_attribute WHERE attrelid = '${s}.podcast'::regclass AND attnum > 0 ORDER BY attnum`)).map((r) => r.attname);
      const v32Columns = await columns();
      assert.ok(!v32Columns.includes("date"), "schema v32 has no podcast.date");
      await db.query(`CREATE VIEW public.my_podcasts AS SELECT key, title, "runningTime" FROM ${s}.podcast`);

      zotero.setSchema("v42"); // Zotero inserted podcast.date (and more) mid-list
      const run = await zotSync(fetchArgs(zotero, "--user", String(USER), "--key", KEY));
      assert.equal(run.code, 0, run.out);
      assert.match(run.out, /up to date at version \d+ · views updated/);

      const v42Columns = await columns();
      assert.deepEqual(v42Columns.slice(0, v32Columns.length), v32Columns, "existing columns keep their place");
      assert.ok(v42Columns.slice(v32Columns.length).includes("date"), "new fields are appended");
      assert.equal((await one(`SELECT count(*)::int AS n FROM public.my_podcasts`)).n, 2);
    },
    "v32"
  );
});

test("a library that changes mid-sync is synced again from the same version", async () => {
  const lib = userLibrary();
  await withZotero([lib], async (zotero) => {
    let changed = false;
    zotero.onRequest((request) => {
      if (!changed && request.path === `/users/${USER}/items` && request.query.format === "versions") {
        changed = true;
        lib.put("items", { key: "LA2T3E4R", itemType: "note", note: "<p>added during the sync</p>", tags: [], relations: {}, collections: [] });
      }
    });
    const run = await zotSync(fetchArgs(zotero, "--user", String(USER), "--key", KEY));
    assert.equal(run.code, 0, run.out);
    assert.match(run.out, /changed while syncing; starting over/);
    assert.equal((await one(`SELECT version FROM zotero.library`)).version, lib.version);
    assert.equal((await one(`SELECT count(*)::int AS n FROM zotero.item WHERE key = 'LA2T3E4R'`)).n, 1);
  });
});

test("when the delete log no longer reaches the last sync, a full reconciliation removes what is gone", async () => {
  const lib = userLibrary();
  await withZotero([lib], async (zotero) => {
    assert.equal((await zotSync(fetchArgs(zotero, "--user", String(USER), "--key", KEY))).code, 0);
    lib.remove("items", "CD4E5F6G");
    lib.put("items", { key: "AF2T3E4R", itemType: "note", note: "<p>after</p>", tags: [], relations: {}, collections: [] });
    lib.deleteLogStart = lib.version + 1; // the server purged its delete log
    const run = await zotSync(fetchArgs(zotero, "--user", String(USER), "--key", KEY));
    assert.equal(run.code, 0, run.out);
    assert.match(run.out, /no longer reaches the last sync; reconciling every object/);
    assert.deepEqual(
      await all(`SELECT key FROM zotero.item WHERE key IN ('CD4E5F6G', 'AF2T3E4R')`),
      [{ key: "AF2T3E4R" }]
    );
    assert.equal((await one(`SELECT count(*)::int AS n FROM zotero.item`)).n, 11);
  });
});

test("rate limits are waited out", async () => {
  const lib = userLibrary();
  await withZotero([lib], async (zotero) => {
    zotero.failNext((r) => r.path === `/users/${USER}/items` && r.query.format === "versions", {
      status: 429,
      headers: { "Retry-After": "1" },
    });
    const run = await zotSync(fetchArgs(zotero, "--user", String(USER), "--key", KEY));
    assert.equal(run.code, 0, run.out);
    assert.match(run.out, /HTTP 429\); waiting 1s/);
    assert.equal((await one(`SELECT count(*)::int AS n FROM zotero.item`)).n, 11);
  });
});

test("--key --all syncs the user library and its groups, with group metadata", async () => {
  await withZotero([userLibrary(), groupLibrary()], async (zotero) => {
    const run = await zotSync(fetchArgs(zotero, "--key", KEY, "--all"));
    assert.equal(run.code, 0, run.out);
    assert.deepEqual(await all(`SELECT id, name, group_data->'data'->>'type' AS type FROM zotero.library ORDER BY id`), [
      { id: `g${GROUP}`, name: "Sync Research Group", type: "Private" },
      { id: `u${USER}`, name: "zot-sync tester", type: null },
    ]);
    assert.equal((await one(`SELECT title FROM zot_g_${GROUP}."journalArticle"`)).title, "Mirroring bibliographies");
  });
});

test("one failing library is recorded and does not stop the others", async () => {
  await withZotero([userLibrary(), groupLibrary({ forbidden: true })], async (zotero) => {
    const run = await zotSync(fetchArgs(zotero, "--key", KEY, "--all"));
    assert.equal(run.code, 1, run.out);
    assert.match(run.out, new RegExp(`zot_g_${GROUP}: FAILED: HTTP 403 from ${zotero.url}/groups/${GROUP}: Forbidden`));
    assert.match(run.out, /1 of 2 libraries could not be synced/);
    assert.deepEqual(await all(`SELECT library, status, error FROM zotero.sync_run ORDER BY id`), [
      { library: `u${USER}`, status: "ok", error: null },
      { library: `g${GROUP}`, status: "failed", error: `HTTP 403 from ${zotero.url}/groups/${GROUP}: Forbidden` },
    ]);
    assert.equal((await one(`SELECT count(*)::int AS n FROM zotero.item WHERE library = 'u${USER}'`)).n, 11);
  });
});

test("large libraries are fetched 100 at a time (the API would page at 25 without limit)", async () => {
  const lib = new FakeLibrary({ type: "user", id: USER, name: "big" });
  const template = fixture("items-top.json")[0].data;
  for (let i = 0; i < 230; i++) lib.put("items", { ...template, key: `BIG${String(i).padStart(5, "0")}`, title: `Item ${i}` });
  await withZotero([lib], async (zotero) => {
    const run = await zotSync(fetchArgs(zotero, "--user", String(USER), "--key", KEY, "--skip-fulltext"));
    assert.equal(run.code, 0, run.out);
    assert.equal((await one(`SELECT count(*)::int AS n FROM zot_u_${USER}."journalArticle"`)).n, 230);
    const batches = zotero.requests.filter((r) => r.query.itemKey !== undefined);
    assert.deepEqual(batches.map((r) => r.query.itemKey.split(",").length), [100, 100, 30]);
    assert.ok(!zotero.requests.some((r) => r.path.endsWith("/fulltext")), "--skip-fulltext skips full text");
  });
});

test("a zot-sync 0.3 database is refused untouched", async () => {
  await db.query("CREATE SCHEMA logs; CREATE TABLE logs.zot_fetch (id serial)");
  await withZotero([userLibrary()], async (zotero) => {
    const run = await zotSync(fetchArgs(zotero, "--user", String(USER)));
    assert.notEqual(run.code, 0);
    assert.match(run.out, /zot-sync 0\.3 layout \(logs\.zot_fetch are tables\)/);
    assert.equal((await one(`SELECT count(*)::int AS n FROM pg_namespace WHERE nspname = 'zotero'`)).n, 0);
  });
});

test("check reports on the database and the key", async () => {
  await withZotero([userLibrary()], async (zotero) => {
    const run = await zotSync(["check", "--db", DB, "--key", KEY, "--api-url", zotero.url]);
    assert.equal(run.code, 0, run.out);
    assert.match(run.out, /Database OK: the zot-sync tables are in place\./);
    assert.match(run.out, /"userID": 1234567/);
    assert.doesNotMatch(run.out, /"key":/);
    const bad = await zotSync(["check", "--key", "WRONG", "--api-url", zotero.url]);
    assert.notEqual(bad.code, 0);
    assert.match(bad.out, /rejected the API key/);
  });
});
