// A small, faithful stand-in for the Zotero Web API v3 sync endpoints.
//
// It models what the real API promises: every change bumps the library version,
// objects carry the version they were last changed at, deletions go into a
// delete log, and responses carry Last-Modified-Version. Like the real API it
// pages multi-object responses at 25 unless `limit` is given.
import http from "node:http";

export class FakeLibrary {
  constructor({ type, id, name, group = null }) {
    this.type = type; // "user" | "group"
    this.id = id;
    this.name = name;
    this.group = group; // metadata served at /groups/<id>
    this.version = 0;
    this.objects = { items: new Map(), collections: new Map(), searches: new Map() };
    this.settings = new Map(); // name -> { value, version }
    this.deletions = []; // { kind, key, version }
    this.fulltext = new Map(); // item key -> { version, data }
    this.deleteLogStart = 0; // `deleted?since=V` with V below this answers 409
  }

  bump() {
    this.version += 1;
    return this.version;
  }

  /** Create or change an object; `data` is an API `data` object. */
  put(kind, data, meta = {}) {
    const version = this.bump();
    this.objects[kind].set(data.key, { data: { ...data, version }, meta });
    return version;
  }

  remove(kind, key) {
    const version = this.bump();
    this.objects[kind].delete(key);
    this.deletions.push({ kind, key, version });
    if (kind === "items") this.fulltext.delete(key);
    return version;
  }

  setSetting(name, value) {
    this.settings.set(name, { value, version: this.bump() });
  }

  removeSetting(name) {
    this.settings.delete(name);
    this.deletions.push({ kind: "settings", key: name, version: this.bump() });
  }

  setFulltext(itemKey, data) {
    this.fulltext.set(itemKey, { version: this.bump(), data });
  }

  get prefix() {
    return `${this.type}s/${this.id}`;
  }

  envelope(kind, key) {
    const { data, meta } = this.objects[kind].get(key);
    return {
      key,
      version: data.version,
      library: { type: this.type, id: this.id, name: this.name, links: {} },
      links: { self: { href: `https://api.zotero.org/${this.prefix}/${kind}/${key}`, type: "application/json" } },
      meta,
      data,
    };
  }
}

/**
 * @param {{ libraries: FakeLibrary[], keys?: Record<string, object>, schemas: Record<string, object>, schema: string }} config
 */
export function startFakeZotero(config) {
  const libraries = new Map(config.libraries.map((l) => [l.prefix, l]));
  const state = {
    schema: config.schema,
    requests: [],
    hooks: [],
    failures: [],
  };

  const server = http.createServer((req, res) => {
    const url = new URL(req.url, "http://fake");
    const parts = url.pathname.split("/").filter(Boolean);
    const q = Object.fromEntries(url.searchParams);
    const request = { method: req.method, path: url.pathname, query: q, headers: req.headers, url: req.url };
    state.requests.push(request);

    const send = (status, body, headers = {}) => {
      const text = typeof body === "string" ? body : JSON.stringify(body);
      res.writeHead(status, { "Content-Type": typeof body === "string" ? "text/plain" : "application/json", ...headers });
      res.end(status === 304 ? undefined : text);
    };

    for (const hook of state.hooks) hook(request);

    const failureIndex = state.failures.findIndex((f) => f.match(request));
    if (failureIndex >= 0) {
      const [failure] = state.failures.splice(failureIndex, 1);
      return send(failure.status, failure.body ?? "injected failure", failure.headers ?? {});
    }

    if (parts[0] === "schema") {
      const { etag, document } = config.schemas[state.schema];
      if (req.headers["if-none-match"] === etag) return send(304, "", { ETag: etag });
      return send(200, document, { ETag: etag });
    }

    const key = req.headers["zotero-api-key"];
    if (parts[0] === "keys" && parts[1] === "current") {
      const info = config.keys?.[key];
      return info ? send(200, { key, ...info }) : send(403, "Forbidden");
    }

    if (parts[0] === "users" && parts[2] === "groups") {
      const groups = [...libraries.values()].filter((l) => l.type === "group" && l.group?.members?.includes(Number(parts[1])));
      return send(200, Object.fromEntries(groups.map((g) => [String(g.id), g.group.version])));
    }

    if (parts[0] === "groups" && parts.length === 2) {
      const lib = libraries.get(`groups/${parts[1]}`);
      if (!lib) return send(404, "Not found");
      if (lib.group?.forbidden) return send(403, "Forbidden");
      return send(200, { id: lib.id, version: lib.group.version, meta: {}, data: { id: lib.id, version: lib.group.version, name: lib.name, ...lib.group } });
    }

    const lib = libraries.get(`${parts[0]}/${parts[1]}`);
    if (!lib) return send(404, "Not found");
    const headers = { "Last-Modified-Version": String(lib.version) };
    const since = Number(q.since ?? 0);
    const rest = parts.slice(2);
    const changedSince = (version) => version > since;
    const includeTrashed = q.includeTrashed === "1";
    const visible = (kind, data) => kind !== "items" || includeTrashed || !data.deleted;

    if (rest[0] === "settings") {
      const ims = req.headers["if-modified-since-version"];
      if (ims !== undefined && lib.version <= Number(ims)) return send(304, "", headers);
      const out = {};
      for (const [name, s] of lib.settings) if (changedSince(s.version)) out[name] = s;
      return send(200, out, headers);
    }

    if (rest[0] === "deleted") {
      if (since < lib.deleteLogStart) return send(409, "Delete log purged", headers);
      const out = { collections: [], searches: [], items: [], tags: [], settings: [] };
      for (const d of lib.deletions) if (changedSince(d.version)) out[d.kind].push(d.key);
      return send(200, out, headers);
    }

    if (rest[0] === "fulltext") {
      const out = {};
      for (const [itemKey, f] of lib.fulltext) if (changedSince(f.version)) out[itemKey] = f.version;
      return send(200, out, headers);
    }

    if (rest[0] === "items" && rest[2] === "fulltext") {
      const f = lib.fulltext.get(rest[1]);
      return f ? send(200, f.data, { "Last-Modified-Version": String(f.version) }) : send(404, "Not found", headers);
    }

    const kind = rest[0];
    if (["items", "collections", "searches"].includes(kind) && rest.length === 1) {
      const objects = lib.objects[kind];
      if (q.format === "versions") {
        const out = {};
        for (const [k, o] of objects) if (changedSince(o.data.version) && visible(kind, o.data)) out[k] = o.data.version;
        return send(200, out, headers);
      }
      const keyParam = { items: "itemKey", collections: "collectionKey", searches: "searchKey" }[kind];
      if (q[keyParam] !== undefined) {
        const limit = Number(q.limit ?? 25);
        const keys = q[keyParam].split(",").filter((k) => objects.has(k) && visible(kind, objects.get(k).data));
        return send(200, keys.slice(0, limit).map((k) => lib.envelope(kind, k)), { ...headers, "Total-Results": String(keys.length) });
      }
    }

    return send(400, `fake Zotero does not implement ${req.url}`);
  });

  return new Promise((resolve) => {
    server.listen(0, "127.0.0.1", () => {
      resolve({
        url: `http://127.0.0.1:${server.address().port}`,
        requests: state.requests,
        setSchema: (name) => (state.schema = name),
        /** Run `fn(request)` for every request (e.g. to change the library mid-sync). */
        onRequest: (fn) => state.hooks.push(fn),
        /** Answer the next request matching `match` with an injected response. */
        failNext: (match, failure) => state.failures.push({ match, ...failure }),
        close: () => new Promise((done) => server.close(done)),
      });
    });
  });
}
