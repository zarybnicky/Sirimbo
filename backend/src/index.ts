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
app.use(require('morgan')('tiny'));
app.use(require('cookie-parser')());

app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: false }));
app.use(bodyParser.text({ type: 'application/graphql' }));

app.use(postgraphile(pool, ['public'], graphileOptions));

app.get('/logout', async function(req, res) {
  const phpsessid = req.cookies.PHPSESSID;
  if (phpsessid) {
    await pool.query(`DELETE FROM session WHERE ss_id='${phpsessid}'`);
  }
  return res.clearCookie('PHPSESSID', { path: '/' }).redirect('/');
});

(async function runWorker() {
  const runner = await run({
    concurrency: 5,
    noHandleSignals: false,
    pollInterval: 1000,
    taskList,
  });
  await runner.promise;
})();

const port = parseInt(process.env.PORT || '3000', 10);
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

process.on('unhandledRejection', (reason, promise) => {
  console.log('Unhandled Rejection at:', reason.stack || reason)
  sys
})
