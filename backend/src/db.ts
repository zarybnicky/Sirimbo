import { Pool } from 'pg';
import { createWithPgClient, PgSubscriber } from "postgraphile/@dataplan/pg/adaptors/pg";

export const pool = new Pool();

const withPgClient = createWithPgClient({ pool });
const pgSubscriber = new PgSubscriber(pool);

export const poolGraphqlContext = { withPgClient, pgSubscriber };
