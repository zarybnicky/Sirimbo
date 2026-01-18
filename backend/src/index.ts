#!/usr/bin/env node

import * as HyperDX from '@hyperdx/node-opentelemetry';

HyperDX.init({
  consoleCapture: false,
  service: 'rozpisovnik-api',
});

import process from 'process';
import express from 'express';
import bodyParser from 'body-parser';
import cookieParser from 'cookie-parser';
import compression from 'compression';
import helmet from 'helmet';
import cors from 'cors';
import morgan from 'morgan';
import { postgraphile } from 'postgraphile';
import preset from './graphile.config.ts';
import { grafserv } from 'postgraphile/grafserv/express/v4';
import { createServer } from 'node:http';
import { installStarletProxy } from './starlet-proxy.ts';
import { authContext, withPgClientAndPgSettings } from './auth.ts';

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

app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: false }));
app.use(bodyParser.text({ type: 'application/graphql' }));

installStarletProxy(app);

app.use(authContext());

app.get('/member/download', async function (req, res) {
  await withPgClientAndPgSettings(req, async (client) => {
    const { rows } = await client.query<{ d_path: string; d_filename: string }>({
      text: 'select * from dokumenty where id=$1',
      values: [req.query.id],
    });
    if (rows.length < 1) {
      res.status(404).send('Nenalezeno');
      return;
    }

    let path = rows[0].d_path;
    path = path.replace('/var/lib/olymp/uploads/', 'uploads/');
    path = path.replace('upload/', 'uploads/');
    if (process.env.TS_NODE_DEV) {
      path = `../${path}`;
    }
    res.download(path, rows[0].d_filename);
  });
});

const server = createServer(app);
server.on('error', (e) => {
  console.error(e);
});

const pgl = postgraphile(preset);
const serv = pgl.createServ(grafserv);

serv.addTo(app, server).catch((e) => {
  console.error(e);
  process.exit(1);
});

server.listen(preset.grafserv?.port ?? 5000, () => {
  const address = server.address();
  if (address === null) {
  } else if (typeof address === 'string') {
    console.log(`PostGraphile listening on ${address} 🚀`);
  } else {
    const href = `http://localhost:${address.port}/graphiql`;
    console.log(`PostGraphiQL available at ${href} 🚀`);
  }
});

process.on('unhandledRejection', (reason) => {
  throw reason;
});
