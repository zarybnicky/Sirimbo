import { Pool } from 'pg';
import process from 'process';
import express from 'express';
import bodyParser from 'body-parser';
import { postgraphile, PostGraphileOptions, makePluginHook, makeExtendSchemaPlugin, gql } from 'postgraphile';
import PgPubsub from '@graphile/pg-pubsub';

const pluginHook = makePluginHook([PgPubsub]);
const MySubscriptionPlugin = makeExtendSchemaPlugin(build => ({
  typeDefs: gql`
    type TimePayload {
      currentTimestamp: String
      query: Query
    }
    extend type Subscription {
      time: TimePayload @pgSubscription(topic: "time")
    }
  `,
  resolvers: {
    Subscription: {
      time: (event) => event,
    },
    TimePayload: {
      query: () => build.$$isQuery,
    },
  },
}));

export const options: PostGraphileOptions = {
  pluginHook,
  appendPlugins: [MySubscriptionPlugin],
  pgSettings(req) {
    // Adding this to ensure that all servers pass through the request in a
    // good enough way that we can extract headers.
    // CREATE FUNCTION current_user_id() RETURNS text AS $$ SELECT current_setting('graphile.test.x-user-id', TRUE); $$ LANGUAGE sql STABLE;
    return {
      'graphile.test.x-user-id':
        req.headers['x-user-id'] ||
        // `normalizedConnectionParams` comes from websocket connections, where
        // the headers often cannot be customized by the client.
        (req as any).normalizedConnectionParams?.['x-user-id'],
    };
  },
  watchPg: true,
  graphiql: true,
  enhanceGraphiql: true,
  subscriptions: true,
  dynamicJson: true,
  setofFunctionsContainNulls: false,
  ignoreRBAC: false,
  showErrorStack: 'json',
  extendedErrors: ['hint', 'detail', 'errcode'],
  allowExplain: true,
  legacyRelations: 'omit',
  sortExport: true,
};

const pool = new Pool();
const app = express();

app.use(require('compression')({ threshold: 0 }));
app.use(require('helmet')());
app.use(require('morgan')('tiny'));
app.use(require('cookie-parser')());

app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: false }));
app.use(bodyParser.text({ type: 'application/graphql' }));

app.use(postgraphile(pool, ['public'], options));

app.get('/graphql-auth', async function(req, res) {
  const phpsessid = req.cookies.PHPSESSID;
  if (!phpsessid) return res.json({ 'X-Hasura-Role': 'anonymous' });

  const sessRes = await pool.query(`SELECT * FROM session WHERE ss_id='${phpsessid}'`);
  if (!sessRes.rows[0]) return res.json({ 'X-Hasura-Role': 'anonymous' });

  const uid = JSON.parse(sessRes.rows[0].ss_data).id
  if (!uid) return res.json({ 'X-Hasura-Role': 'anonymous' });

  const userRes = await pool.query(`SELECT * FROM users WHERE u_id=${uid}`);
  if (!userRes.rows[0]) return res.json({ 'X-Hasura-Role': 'anonymous' });

  return res.json({
    'X-Hasura-Role': (
      userRes.rows[0].u_group == "0" ? "anonymous" :
        userRes.rows[0].u_group == "1" ? "admin" :
          "member"
    ),
    'X-Hasura-User-Id': uid.toString(),
  })
});

app.get('/logout', async function(req, res) {
  const phpsessid = req.cookies.PHPSESSID;
  if (phpsessid) {
    await pool.query(`DELETE FROM session WHERE ss_id='${phpsessid}'`);
  }
  return res.clearCookie('PHPSESSID', { path: '/' }).redirect('/');
});

const port: number = process.env.PORT ? parseInt(process.env.PORT, 10) : 3000;
const server = app.listen(port, () => {
  const address = server.address();
  if (address === null) {
  } else if (typeof address === 'string') {
    console.log(`PostGraphile listening on ${address} 🚀`);
  } else {
    const href = `http://localhost:${address.port}${options.graphiqlRoute || '/graphiql'}`;
    console.log(`PostGraphiQL available at ${href} 🚀`);
  }
});
