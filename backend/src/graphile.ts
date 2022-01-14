import express from 'express';
import { makePluginHook, Build, PostGraphileOptions } from 'postgraphile';
import operationHooks, { AddOperationHookFn, OperationHookGenerator } from '@graphile/operation-hooks';
import { getSession, getUser } from './db';
import { UploadPlugin } from './minio';

const useAuthCredentials: (build: Build) => OperationHookGenerator = _ => ctx => {
  if (ctx.scope.isRootMutation && ctx.scope.pgFieldIntrospection?.name === "login") {
    return {
      after: [{
        priority: 1000,
        callback: (result, _, context) => {
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
        callback: (result, _, context) => {
          context.unsetAuthCookie();
          return result;
        },
      }],
    };
  }

  return {};
};

const loadUserFromSession = async (req: express.Request): Promise<{ [k: string]: any }> => {
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
}

export const graphileOptions: PostGraphileOptions<express.Request, express.Response> = {
  // subscriptions: true,
  retryOnInitFail: true,
  dynamicJson: true,
  setofFunctionsContainNulls: false,
  ignoreRBAC: false,
  extendedErrors: ['hint', 'detail', 'errcode'],
  showErrorStack: "json",
  pgSettings: loadUserFromSession,
  watchPg: true,
  graphiql: true,
  enhanceGraphiql: true,
  allowExplain: true,
  legacyRelations: 'omit',
  sortExport: true,
  enableQueryBatching: true,
  pluginHook: makePluginHook([operationHooks]),

  async additionalGraphQLContextFromRequest(_, res) {
    return {
      setAuthCookie: (sessionId: string) => {
        res.cookie('PHPSESSID', sessionId, { sameSite: true, httpOnly: true, secure: true });
      },
      unsetAuthCookie: () => {
        res.clearCookie('PHPSESSID');
      },
    };
  },

  appendPlugins: [
    require("@graphile-contrib/pg-simplify-inflector"),
    UploadPlugin,
    function OperationHookPlugin(builder) {
      builder.hook("init", (_, build) => {
        const addOperationHook: AddOperationHookFn = build.addOperationHook;
        addOperationHook(useAuthCredentials(build));
        return _;
      });
    },
  ],
};

export default graphileOptions;
