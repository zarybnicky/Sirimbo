import express from 'express';
import { PostGraphileOptions, makeExtendSchemaPlugin } from 'postgraphile';
import path from 'path';
import { pool } from './db';
import { gql, makeWrapResolversPlugin } from 'graphile-utils';
import { NodePlugin } from 'graphile-build';
import { DeleteObjectCommand, GetObjectCommand, PutObjectCommand, S3Client } from '@aws-sdk/client-s3';
import { getSignedUrl } from '@aws-sdk/s3-request-presigner';
import { JwtPayload, verify as verifyJwt } from 'jsonwebtoken';

const s3client = new S3Client({
  region: process.env.S3_REGION,
  endpoint: process.env.S3_ENDPOINT,
  forcePathStyle: true,
});
const bucketName = process.env.S3_BUCKET!
const s3publicEndpoint = process.env.S3_PUBLIC_ENDPOINT || process.env.S3_ENDPOINT;

async function loadUserFromSession(req: express.Request): Promise<{ [k: string]: any }> {
  let tenantId = '1';
  if (req.headersDistinct['x-tenant-id']?.length) {
    tenantId = req.headersDistinct['x-tenant-id'][0];
  } else {
    const {
      rows: [host],
    } = await pool.query(
      'select id from tenant where $1 = any (origins) or $2 = any (origins)',
      [req.headers.host, req.headers.origin],
    );
    if (host) {
      tenantId = host.id;
    }
  }

  const authorization = req.get('authorization')
  if (authorization?.toLowerCase().startsWith('bearer ')) {
    const token = authorization.substring(7)
    const claims = verifyJwt(token, process.env.JWT_SECRET || '', {
      ignoreExpiration: true
    }) as JwtPayload;
    const settings: Record<string, string> = {
      role: claims.is_admin ? 'administrator' : claims.is_member ? 'member' : 'anonymous',
    };

    for (const key in claims) {
      if (['exp', 'aud', 'iat', 'iss'].includes(key)) continue
      if (Array.isArray(claims[key])) {
        settings[`jwt.claims.${key}`] = '[' + claims[key].map((x: string) => `${x}`).join(',') + ']';
      } else {
        settings[`jwt.claims.${key}`] = claims[key];
      }
    }
    //DEBUG: console.log('select ' + Object.entries(settings).map(([k, v]) => `set_config('${k}', '${v}', true)`).join(', ') + ';');
    return settings;
  } else {
    return {
      role: 'anonymous',
      'jwt.claims.tenant_id': tenantId,
    };
  }
}

const isDevelopment = process.env.NODE_ENV === 'development';

export const graphileOptions: PostGraphileOptions<express.Request, express.Response> & {
  connectionFilterAllowedOperators?: string[];
  connectionFilterAllowedFieldTypes?: string[];
  connectionFilterArrays?: boolean;
  connectionFilterComputedColumns?: boolean;
  connectionFilterOperatorNames?: boolean;
  connectionFilterRelations?: boolean;
  connectionFilterSetofFunctions?: boolean;
  connectionFilterLogicalOperators?: boolean;
  connectionFilterAllowNullInput?: boolean;
  connectionFilterAllowEmptyObjectInput?: boolean;
} = {
  // subscriptions: true,
  retryOnInitFail: true,
  dynamicJson: true,
  setofFunctionsContainNulls: false,
  ignoreRBAC: false,
  extendedErrors: ['hint', 'detail', 'errcode', 'where'],
  showErrorStack: 'json',
  pgSettings: loadUserFromSession,
  watchPg: true,
  legacyRelations: 'omit',
  sortExport: true,
  enableQueryBatching: true,

  jwtPgTypeIdentifier: 'public.jwt_token',
  jwtSecret: process.env.JWT_SECRET,

  graphiql: isDevelopment,
  enhanceGraphiql: isDevelopment,
  allowExplain: isDevelopment,
  exportGqlSchemaPath: isDevelopment ? path.resolve('../schema.graphql') : undefined,

  connectionFilterArrays: false,
  connectionFilterComputedColumns: false,
  connectionFilterRelations: false,
  connectionFilterSetofFunctions: true,
  connectionFilterLogicalOperators: true,
  connectionFilterAllowNullInput: false,
  connectionFilterAllowEmptyObjectInput: false,

  skipPlugins: [NodePlugin],

  appendPlugins: [
    // require('postgraphile-plugin-connection-filter'),
    require('@graphile-contrib/pg-simplify-inflector'),
    require('@graphile-contrib/pg-order-by-related'),
    makeWrapResolversPlugin({
      Mutation: {
        deleteAttachment: {
          async resolve(resolve, _source, args, _context, _resolveInfo) {
            const result = await (resolve as any)();
            s3client.send(new DeleteObjectCommand({ Key: args.input.objectName, Bucket: bucketName }));
            return result;
          },
        }
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
          publicUrl: ({ objectName }) => `${s3publicEndpoint}/${bucketName}/${objectName}`,
        },
      },
    })),
  ],
};

export default graphileOptions;
