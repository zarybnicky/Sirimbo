#!/usr/bin/env node

import { chdir } from 'node:process';
chdir(import.meta.dirname);

import process from 'process';
import express from 'express';
import cookieParser from 'cookie-parser';
import compression from 'compression';
import helmet from 'helmet';
import cors from 'cors';
import morgan from 'morgan';
import { postgraphile } from 'postgraphile';
import preset from './graphile.config.ts';
import { grafserv } from 'postgraphile/grafserv/express/v4';
import { createServer } from 'node:http';
import { authContext } from './auth.ts';
import { pool } from './db.ts';

const app = express();

app.use(compression({ threshold: 0 }));
app.use(helmet());
app.use(cors({ origin: true, credentials: true }));
app.use(cookieParser());
app.use(morgan('tiny'));

app.use(express.json());
app.use(express.urlencoded({ extended: false }));
app.use(express.text({ type: 'application/graphql' }));

app.use(authContext());

const server = createServer(app);
const graphile = postgraphile(preset);

server.on('listening', () => {
  const address = server.address();
  if (typeof address === 'string') {
    console.log(`PostGraphile listening on ${address}`);
  } else if (address !== null) {
    console.log(`PostGraphile listening on ${address.port}`);
  }
});

process.on('unhandledRejection', (e) => {
  throw e;
});

await graphile.getSchema();
await graphile.createServ(grafserv).addTo(app, server);

if (process.env.LISTEN_FDS) {
  server.listen({ fd: 3 });
} else {
  server.listen(preset.grafserv?.port ?? 5200);
}

function shutdown() {
  server.close(async (e) => {
    if (e) throw e;
    await graphile.release();
    await pool.end();
    process.exit(0);
  });
}
process.once('SIGTERM', shutdown);
process.once('SIGINT', shutdown);
