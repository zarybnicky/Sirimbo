import { Pool } from 'pg';
import process from 'process';
import express from 'express';
import bodyParser from 'body-parser';
import { postgraphile, makePluginHook, PostGraphileOptions } from 'postgraphile';
import operationHooks, { AddOperationHookFn } from '@graphile/operation-hooks';

export const options: PostGraphileOptions<express.Request, express.Response> = {
  async pgSettings(req) {
    const phpsessid = (req as any).cookies.PHPSESSID;
    if (!phpsessid) return { 'role': 'anonymous' };

    const sessRes = await pool.query(`SELECT * FROM session WHERE ss_id='${phpsessid}'`);
    if (!sessRes.rows[0]) return { 'role': 'anonymous' };

    const uid = JSON.parse(sessRes.rows[0].ss_data).id
    if (!uid) return { 'role': 'anonymous' };

    const userRes = await pool.query(`SELECT * FROM users WHERE u_id=${uid}`);
    if (!userRes.rows[0]) return { 'role': 'anonymous' };

    return {
      'role': (
        userRes.rows[0].u_group == "0" ? "anonymous" :
          userRes.rows[0].u_group == "1" ? "administrator" :
            "member"
      ),
      'jwt.claims.user_id': uid.toString(),
    };
  },
  watchPg: true,
  graphiql: true,
  enhanceGraphiql: true,
  // subscriptions: true,
  dynamicJson: true,
  setofFunctionsContainNulls: false,
  ignoreRBAC: false,
  showErrorStack: 'json',
  extendedErrors: ['hint', 'detail', 'errcode'],
  allowExplain: true,
  legacyRelations: 'omit',
  sortExport: true,
  pluginHook: makePluginHook([operationHooks]),
  async additionalGraphQLContextFromRequest(_, res) {
    return {
      setPhpsessid: (sessionId: string) => {
        res.cookie('PHPSESSID', sessionId, { sameSite: true, httpOnly: true, secure: true });
      },
    };
  },
  appendPlugins: [
    function OperationHookPlugin(builder) {
      builder.hook("init", (_, build) => {
        (build.addOperationHook as AddOperationHookFn)(({ isRootMutation, pgFieldIntrospection }) => {
          if (!isRootMutation) {
            return {};
          }
          if (!pgFieldIntrospection || pgFieldIntrospection.name !== "login") {
            return {};
          }
          return {
            after: [{
              priority: 1000,
              callback: (result, args, context) => {
                console.log(result);
                context.setAuthCookie(result)
              },
            }],
          };
        });
        return _;
      });
    },
  ],
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
