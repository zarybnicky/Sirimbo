#!/usr/bin/env node

import process from 'process';
import express from 'express';
import bodyParser from 'body-parser';
import { postgraphile } from 'postgraphile';
import { run } from "graphile-worker";
import { pool } from './db';
import { graphileOptions } from './graphile';
import taskList from './tasks';

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

app.use(postgraphile(pool, ['public'], graphileOptions));

(async function runWorker() {
  const runner = await run({
    concurrency: 5,
    noHandleSignals: false,
    pollInterval: 1000,
    taskList,
  });
  await runner.promise;
})();

const port = parseInt(process.env.PORT || '5000', 10);
const server = app.listen(port, () => {
  const address = server.address();
  if (address === null) {
  } else if (typeof address === 'string') {
    console.log(`PostGraphile listening on ${address} 🚀`);
  } else {
    const href = `http://localhost:${address.port}${graphileOptions.graphiqlRoute || '/graphiql'}`;
    console.log(`PostGraphiQL available at ${href} 🚀`);
  }
});

process.on('unhandledRejection', (reason) => { throw reason; });
