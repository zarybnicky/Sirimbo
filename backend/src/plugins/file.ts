import { getSignedUrl } from '@aws-sdk/s3-request-presigner';
import {
  DeleteObjectCommand,
  GetObjectCommand,
  PutObjectCommand,
  S3Client,
} from '@aws-sdk/client-s3';
import { gql, makeWrapResolversPlugin } from 'graphile-utils';
import { Plugin } from 'graphile-build';
import { makeExtendSchemaPlugin } from 'postgraphile';

const s3client = new S3Client({
  region: process.env.S3_REGION,
  endpoint: process.env.S3_ENDPOINT,
  forcePathStyle: true,
});
const bucketName = process.env.S3_BUCKET!;
const s3publicEndpoint = process.env.S3_PUBLIC_ENDPOINT || process.env.S3_ENDPOINT;

const plugins: Plugin[] = [
  makeWrapResolversPlugin({
    Mutation: {
      deleteAttachment: {
        async resolve(resolve, _source, args, _context, _resolveInfo) {
          const result = await (resolve as any)();
          s3client.send(
            new DeleteObjectCommand({ Key: args.input.objectName, Bucket: bucketName }),
          );
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
        uploadUrl: ({ objectName }) => getSignedUrl(
          s3client,
          new PutObjectCommand({ Key: objectName, Bucket: bucketName }),
        ),
        downloadUrl: ({ objectName }) => getSignedUrl(
          s3client,
          new GetObjectCommand({ Key: objectName, Bucket: bucketName }),
        ),
        publicUrl: ({ objectName }) => `${s3publicEndpoint}/${bucketName}/${objectName}`,
      },
    },
  })),
];

export default plugins;
