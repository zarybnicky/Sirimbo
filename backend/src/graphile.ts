import express from 'express';
import { makePluginHook, Build, PostGraphileOptions, makeExtendSchemaPlugin } from 'postgraphile';
import operationHooks, { AddOperationHookFn, OperationHookGenerator } from '@graphile/operation-hooks';
import path from 'path';
import * as Minio from 'minio';
import { pool } from './db';
import { gql } from 'graphile-utils';

const minioClient = new Minio.Client({
  endPoint: process.env.MINIO_DOMAIN!,
  port: parseInt(process.env.MINIO_PORT!, 10),
  accessKey: process.env.MINIO_ACCESS_KEY!,
  secretKey: process.env.MINIO_SECRET_KEY!,
  useSSL: false,
  pathStyle: true,
});

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

  const { rows: [session] } = await pool.query(
    `SELECT u_id, u_group, ss_id FROM session LEFT JOIN users on u_id=ss_user WHERE ss_id='${req.cookies.PHPSESSID}'`
  );
  if (session) {
    settings['jwt.claims.session_id'] = session.ss_id;
    settings['jwt.claims.user_id'] = session.u_id;;

    settings['role'] =
      session.u_group == "0" ? "anonymous" :
        session.u_group == "1" ? "administrator" :
          "member";
  }

  return settings;
}

const isDevelopment = !!process.env.DEBUG;

export const graphileOptions: PostGraphileOptions<express.Request, express.Response> = {
  // subscriptions: true,
  retryOnInitFail: true,
  dynamicJson: true,
  setofFunctionsContainNulls: false,
  ignoreRBAC: false,
  extendedErrors: ['hint', 'detail', 'errcode', 'where'],
  showErrorStack: "json",
  pgSettings: loadUserFromSession,
  watchPg: true,
  legacyRelations: 'omit',
  sortExport: true,
  enableQueryBatching: true,
  pluginHook: makePluginHook([operationHooks]),

  graphiql: isDevelopment,
  enhanceGraphiql: isDevelopment,
  allowExplain: isDevelopment,
  exportGqlSchemaPath: isDevelopment ? path.resolve('../schema.graphql') : undefined,

  async additionalGraphQLContextFromRequest(_, res) {
    return {
      setAuthCookie: (sessionId: string) => {
        res.cookie('PHPSESSID', sessionId, { sameSite: true, httpOnly: true });
      },
      unsetAuthCookie: () => {
        res.clearCookie('PHPSESSID');
      },
    };
  },

  appendPlugins: [
    require("@graphile-contrib/pg-simplify-inflector"),
    function OperationHookPlugin(builder) {
      builder.hook("init", (_, build) => {
        const addOperationHook: AddOperationHookFn = build.addOperationHook;
        addOperationHook(useAuthCredentials(build));
        return _;
      });
    },
    makeExtendSchemaPlugin(_build => ({
      typeDefs: gql`
type UploadFilePayload {
  uploadUrl: String!
  objectName: String!
}

extend type Mutation {
  uploadFile(fileName: String!): UploadFilePayload!
  downloadFile(id: Int!): String!
}`,
      resolvers: {
        Mutation: {
          uploadFile: async (_query, { fileName }, context) => {
            // check auth
            const objectName = `${Date.now()}-${fileName}`;
            await context.pgClient.query('INSERT INTO attachment (object_name) VALUES ($1)', [objectName]);
            const uploadUrl = await minioClient.presignedPutObject('public', objectName);
            return { objectName, uploadUrl };
          },
          downloadFile: async (_query, { id }, context) => {
            // check auth
            const { rows: [file] } = await context.pgClient.query(
              'SELECT * FROM attachment where id=$1', [id],
            );
            return await minioClient.presignedGetObject('public', file.object_name);
          },
        },
      },
    })),
  ],
};

export default graphileOptions;
