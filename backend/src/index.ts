import { Pool } from 'pg';
import process from 'process';
import express from 'express';
import bodyParser from 'body-parser';
import { postgraphile, makePluginHook, PostGraphileOptions, Build } from 'postgraphile';
import operationHooks, { AddOperationHookFn, OperationHookGenerator } from '@graphile/operation-hooks';

const getSession = async (phpsessid: string | undefined) => {
  const sessRes = await pool.query(`SELECT * FROM session WHERE ss_id='${phpsessid}'`);
  return sessRes.rows[0];
};
const getUser = async (uid: string | undefined) => {
  const userRes = await pool.query(`SELECT * FROM users WHERE u_id=${uid}`);
  return userRes.rows[0];
};

export const options: PostGraphileOptions<express.Request, express.Response> = {
  async pgSettings(req) {
    const settings = {
      role: 'anonymous',
      'jwt.claims.session_id': null,
      'jwt.claims.user_id': null,
    };
    const session = await getSession(req.cookies.PHPSESSID)
    if (!session) return settings;
    settings['jwt.claims.session_id'] = session.ss_id;

    const user = await getUser(session.ss_user);
    if (!user) return settings;
    settings['jwt.claims.user_id'] = user.u_id;;

    settings['role'] =
      user.u_group == "0" ? "anonymous" :
        user.u_group == "1" ? "administrator" :
          "member"

    return settings;
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
      setAuthCookie: (sessionId: string) => {
        res.cookie('PHPSESSID', sessionId, { sameSite: true, httpOnly: true, secure: true });
      },
      unsetAuthCookie: () => {
        res.clearCookie('PHPSESSID');
      }
    };
  },
  appendPlugins: [
    function OperationHookPlugin(builder) {
      builder.hook("init", (_, build) => {
        (build.addOperationHook as AddOperationHookFn)(useAuthCredentials(build));
        return _;
      });
    },
  ],
};

const useAuthCredentials: (build: Build) => OperationHookGenerator = build => ctx => {
  if (ctx.scope.isRootMutation && ctx.scope.pgFieldIntrospection?.name === "login") {
    return {
      after: [{
        priority: 1000,
        callback: (result, args, context) => {
          console.log(result.data.value);
          context.setAuthCookie(result.data.value.sess.ss_id);
          return result;
        },
      }],
    };
  }
  if (ctx.scope.isRootMutation && ctx.scope.pgFieldIntrospection?.name === "logout") {
    return {
      after: [{
        priority: 1000,
        callback: (result, args, context) => {
          context.unsetAuthCookie();
          return result;
        },
      }],
    };
  }
  return {};
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
