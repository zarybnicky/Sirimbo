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

const app = express();

app.use(compression({ threshold: 0 }));
app.use(helmet());
app.use(
  cors({
    origin: true,
    credentials: true,
  }),
);
app.use(cookieParser());
app.use(morgan('tiny'));

app.use(express.json());
app.use(express.urlencoded({ extended: false }));
app.use(express.text({ type: 'application/graphql' }));

app.use(authContext());

const server = createServer(app);
server.on('error', (e) => {
  console.error(e);
});


postgraphile(preset)
  .createServ(grafserv)
  .addTo(app, server)
  .catch((e) => {
    console.error(e);
    process.exit(1);
  });

server.listen(preset.grafserv?.port ?? 5200, () => {
  const address = server.address();
  if (typeof address === 'string') {
    console.log(`PostGraphile listening on ${address} 🚀`);
  } else if (address !== null) {
    const href = `http://localhost:${address.port}/graphiql`;
    console.log(`PostGraphiQL available at ${href} 🚀`);
  }
});

process.on('unhandledRejection', (reason) => {
  throw reason;
});
