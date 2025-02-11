#!/usr/bin/env node

import process from 'process';
import express from 'express';
import bodyParser from 'body-parser';
import { postgraphile } from 'postgraphile';
import { pool } from './db.js';
import preset from './graphile.config.js';
import { grafserv } from "grafserv/express/v4";
import { createServer } from "node:http";

const app = express();

app.use(require('compression')({ threshold: 0 }));
app.use(require('helmet')());
app.use(require('cors')());
app.use(require('morgan')('tiny'));

app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: false }));
app.use(bodyParser.text({ type: 'application/graphql' }));

app.get('/member/download', async function (req, res) {
  const {rows} = await pool.query('select * from dokumenty where d_id=$1', [req.query.id]);
  if (rows.length < 1 ) {
    res.status(404).send('Nenalezeno');
    return
  }

  let path = rows[0].d_path;
  path = path.replace('/var/lib/olymp/uploads/', 'uploads/');
  path = path.replace('upload/', 'uploads/');
  if (process.env.TS_NODE_DEV) {
    path = `../${path}`;
  }
  res.download(path, rows[0].d_filename);
});


const server = createServer(app);
server.on("error", (e) => {
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

process.on('unhandledRejection', (reason) => { throw reason; });
