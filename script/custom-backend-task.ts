// Postgres bridge for the zot-sync elm-pages script.
//
// Elm owns all logic and builds SQL + parameters as plain data; this file only
// executes it. SQL errors are returned as data ({ ok: false, error }) instead
// of being thrown, so the Elm side decides what a failure means (e.g. mark one
// library's sync as failed and carry on with the next), and the test suite can
// simulate failures with Test.BackendTask.simulateCustom.

import pg from "pg";

type Statement = { sql: string; params?: unknown[] };
type Rows = { rows: Record<string, unknown>[]; rowCount: number };
type Failure = {
  ok: false;
  error: { message: string; code?: string; detail?: string; hint?: string; statement?: string };
};

const pools = new Map<string, pg.Pool>();

function pool(connectionString: string): pg.Pool {
  let p = pools.get(connectionString);
  if (!p) {
    // allowExitOnIdle: don't keep the CLI process alive once the script is done.
    p = new pg.Pool({ connectionString, allowExitOnIdle: true, max: 4 });
    pools.set(connectionString, p);
  }
  return p;
}

function toRows(r: pg.QueryResult): Rows {
  return { rows: r.rows, rowCount: r.rowCount ?? 0 };
}

function failure(error: unknown, statement?: Statement): Failure {
  const e = (error ?? {}) as { message?: string; code?: string; detail?: string; hint?: string };
  return {
    ok: false,
    error: {
      message: e.message ?? String(error),
      ...(e.code ? { code: e.code } : {}),
      ...(e.detail ? { detail: e.detail } : {}),
      ...(e.hint ? { hint: e.hint } : {}),
      ...(statement ? { statement: statement.sql.slice(0, 300) } : {}),
    },
  };
}

/** Run one statement on any pooled connection. */
export async function pgQuery(input: { db: string } & Statement): Promise<({ ok: true } & Rows) | Failure> {
  try {
    return { ok: true, ...toRows(await pool(input.db).query(input.sql, input.params ?? [])) };
  } catch (error) {
    return failure(error, input);
  }
}

/** Run statements atomically on a single connection: all commit or none do. */
export async function pgTransaction(input: {
  db: string;
  statements: Statement[];
}): Promise<{ ok: true; results: Rows[] } | Failure> {
  let client: pg.PoolClient;
  try {
    client = await pool(input.db).connect();
  } catch (error) {
    return failure(error);
  }
  let current: Statement | undefined;
  try {
    await client.query("BEGIN");
    const results: Rows[] = [];
    for (const s of input.statements) {
      current = s;
      results.push(toRows(await client.query(s.sql, s.params ?? [])));
    }
    await client.query("COMMIT");
    return { ok: true, results };
  } catch (error) {
    await client.query("ROLLBACK").catch(() => undefined);
    return failure(error, current);
  } finally {
    client.release();
  }
}
