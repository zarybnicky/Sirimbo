import path from 'path';
import process from 'process';
import { Client } from 'minio';
import { Plugin } from 'postgraphile';
import { makeExtendSchemaPlugin, gql } from 'graphile-utils';

const port = parseInt(process.env.MINIO_PORT || (process.env.SSL ? '443' : '80'), 10);
const domain = process.env.MINIO_DOMAIN || 'localhost';

const client = new Client({
  endPoint: domain,
  port,
  useSSL: !!process.env.SSL,
  accessKey: process.env.MINIO_ACCESS_KEY || '',
  secretKey: process.env.MINIO_SECRET_KEY || '',
});

export const createPresignedUrl = async (bucket: string, path: string) => {
  return await client.presignedPutObject(bucket, path);
};

export const UploadPlugin: Plugin = makeExtendSchemaPlugin(() => {
  return {
    typeDefs: gql`
      input UploadInput {
        directory: String
        filename: String!
      }
      type Upload {
        uploadUrl: String!
      }
      extend type Mutation {
        uploadFile(input: UploadInput!): Upload!
      }
    `,
    resolvers: {
      Mutation: {
        async uploadFile(query, { input }) {
          const filepath = path.join(input.directory || '', input.filename);
          const uploadUrl = await createPresignedUrl('public', filepath);
          return { uploadUrl };
        },
      },
    },
  };
});
