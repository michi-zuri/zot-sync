// Postgres bridge for the zot-sync elm-pages script.
//
// Elm owns all logic and builds SQL + parameters as plain data; this file only
// executes it. Keeping it this thin is what lets the Elm test suite simulate
// every database interaction with Test.BackendTask.simulateCustom.

import pg from "pg";

type Statement = { sql: string; params?: unknown[] };
type QueryResult = { rows: Record<string, unknown>[]; rowCount: number };

const pools = new Map<string, pg.Pool>();

function pool(connectionString: string): pg.Pool {
  let p = pools.get(connectionString);
  if (!p) {
    // allowExitOnIdle: don't keep the CLI process alive once the script is done.
    p = new pg.Pool({ connectionString, allowExitOnIdle: true });
    pools.set(connectionString, p);
  }
  return p;
}

function toResult(r: pg.QueryResult): QueryResult {
  return { rows: r.rows, rowCount: r.rowCount ?? 0 };
}

/** Run one statement on any pooled connection. */
export async function pgQuery(input: { db: string } & Statement): Promise<QueryResult> {
  return toResult(await pool(input.db).query(input.sql, input.params ?? []));
}

/** Run statements atomically on a single connection: all commit or none do. */
export async function pgTransaction(input: {
  db: string;
  statements: Statement[];
}): Promise<{ results: QueryResult[] }> {
  const client = await pool(input.db).connect();
  try {
    await client.query("BEGIN");
    const results: QueryResult[] = [];
    for (const s of input.statements) {
      results.push(toResult(await client.query(s.sql, s.params ?? [])));
    }
    await client.query("COMMIT");
    return { results };
  } catch (error) {
    await client.query("ROLLBACK");
    throw error instanceof Error ? error.message : error;
  } finally {
    client.release();
  }
}
