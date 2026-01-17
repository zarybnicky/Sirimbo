#!/usr/bin/env node

import process from 'process';
import express from 'express';
import bodyParser from 'body-parser';
import cookieParser from 'cookie-parser';
import compression from 'compression';
import helmet from 'helmet';
import cors from 'cors';
import morgan from 'morgan';
import { postgraphile } from 'postgraphile';
import { pool } from './db.ts';
import preset from './graphile.config.ts';
import { grafserv } from 'postgraphile/grafserv/express/v4';
import { createServer } from 'node:http';
import proxy from 'express-http-proxy';

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

app.get('/member/download', async function (req, res) {
  const { rows } = await pool.query('select * from dokumenty where id=$1', [
    req.query.id,
  ]);
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

app.use(
  '/starlet/graphql',
  cors({
    origin: true,
    credentials: true,
  }),
  proxy('https://evidence.tsstarlet.com', {
    parseReqBody: true,
    proxyReqPathResolver(req) {
      if (req.body && typeof req.body === 'object' && req.body['query'] === '') {
        return '/spa_auth/login';
      }
      return '/graphql';
    },
    proxyReqOptDecorator(proxyReqOpts, srcReq) {
      const auth = srcReq.header('authorization') ?? '';
      const token = auth.replace(/^Bearer\s+/i, '').trim();
      delete proxyReqOpts.headers.authorization;
      proxyReqOpts.headers.cookie = token ? `auth=${encodeURIComponent(token)}` : '';

      proxyReqOpts.headers.host = 'evidence.tsstarlet.com';
      proxyReqOpts.headers.origin = 'https://evidence.tsstarlet.com';
      proxyReqOpts.headers.referer = 'https://evidence.tsstarlet.com';
      return proxyReqOpts;
    },
    proxyReqBodyDecorator(body) {
      if (body && typeof body === 'object' && body['query'] === '') {
        return body['variables'];
      }
      return body;
    },
  }),
);

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
