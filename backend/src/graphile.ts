import express from 'express';
import {PostGraphileOptions, makeExtendSchemaPlugin} from 'postgraphile';
import path from 'path';
import { pool } from './db';
import { gql, makeWrapResolversPlugin } from 'graphile-utils';
import { NodePlugin } from 'graphile-build';
import { GetObjectCommand, PutObjectCommand, S3Client } from '@aws-sdk/client-s3';
import { getSignedUrl } from '@aws-sdk/s3-request-presigner';

const s3client = new S3Client({
  region: process.env.S3_REGION,
  endpoint: process.env.S3_ENDPOINT,
  forcePathStyle: true,
});
const bucketName = process.env.S3_BUCKET!

async function loadUserFromSession(req: express.Request): Promise<{ [k: string]: any }> {
  const settings = {
    role: 'anonymous',
    'jwt.claims.session_id': null,
    'jwt.claims.user_id': null,
    'jwt.claims.tenant_id': '1',
  };

  if (req.headersDistinct['x-tenant-id']?.length) {
    settings['jwt.claims.tenant_id'] = req.headersDistinct['x-tenant-id'][0];
  } else {
    const {
      rows: [host],
    } = await pool.query(
      'select id from tenant where $1 = any (origins) or $2 = any (origins)',
      [req.headers.host, req.headers.origin],
    );
    if (host) {
      settings['jwt.claims.tenant_id'] = host.id;
    }
  }

  const {
    rows: [session],
  } = await pool.query(
    `SELECT u_id, u_group, ss_id
     FROM session LEFT JOIN users on u_id=ss_user
     WHERE ss_id=$1`,
    [req.cookies.PHPSESSID],
  );
  if (session) {
    settings['jwt.claims.session_id'] = session.ss_id;
    settings['jwt.claims.user_id'] = session.u_id;

    settings['role'] =
      session.u_group == '0'
        ? 'anonymous'
        : session.u_group == '1'
        ? 'administrator'
        : 'member';
  }

  return settings;
}

const isDevelopment = process.env.NODE_ENV === 'development';

export const graphileOptions: PostGraphileOptions<express.Request, express.Response> = {
  // subscriptions: true,
  retryOnInitFail: true,
  dynamicJson: true,
  setofFunctionsContainNulls: false,
  ignoreRBAC: false,
  ignoreIndexes: false,
  extendedErrors: ['hint', 'detail', 'errcode', 'where'],
  showErrorStack: 'json',
  pgSettings: loadUserFromSession,
  watchPg: true,
  legacyRelations: 'omit',
  sortExport: true,
  enableQueryBatching: true,

  graphiql: isDevelopment,
  enhanceGraphiql: isDevelopment,
  allowExplain: isDevelopment,
  exportGqlSchemaPath: isDevelopment ? path.resolve('../schema.graphql') : undefined,

  async additionalGraphQLContextFromRequest(_req, res) {
    return {
      setAuthCookie: (sessionId: string) => {
        res.cookie('PHPSESSID', sessionId, {
          sameSite: isDevelopment ? 'lax' : 'none',
          httpOnly: true,
          secure: !isDevelopment,
        });
      },
      unsetAuthCookie: () => {
        res.clearCookie('PHPSESSID');
      },
    };
  },

  skipPlugins: [NodePlugin],

  appendPlugins: [
    require('@graphile-contrib/pg-simplify-inflector'),
    require('@graphile-contrib/pg-order-by-related'),
    makeWrapResolversPlugin({
      Mutation: {
        login: {
          async resolve(resolver, _parent, _args, context, _resolveInfo) {
            const result = await (resolver as any)();
            context.setAuthCookie(result.data.value.sess.ss_id);
            return result;
          },
        },
        logout: {
          async resolve(resolver, _parent, _args, context, _resolveInfo) {
            const result = await (resolver as any)();
            context.unsetAuthCookie();
            return result;
          },
        },
      },
    }),
    makeExtendSchemaPlugin((_build) => ({
      typeDefs: gql`
        extend type Attachment {
          uploadUrl: String! @requires(columns: ["object_name"])
          downloadUrl: String! @requires(columns: ["object_name"])
          publicUrl: String! @requires(columns: ["object_name"])
        }
      `,
      resolvers: {
        Attachment: {
          uploadUrl: ({ objectName }) => getSignedUrl(s3client, new PutObjectCommand({ Key: objectName, Bucket: bucketName })),
          downloadUrl: ({ objectName }) => getSignedUrl(s3client, new GetObjectCommand({ Key: objectName, Bucket: bucketName })),
          publicUrl: ({ objectName }) => `${process.env.S3_ENDPOINT}/${process.env.S3_BUCKET}/${objectName}`,
        },
      },
    })),
  ],
};

export default graphileOptions;
