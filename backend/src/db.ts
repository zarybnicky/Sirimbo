import pg from 'pg';

export const pool = new pg.Pool({ statement_timeout: 5000 });
