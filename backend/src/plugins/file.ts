import { getSignedUrl } from '@aws-sdk/s3-request-presigner';
import {
  DeleteObjectCommand,
  GetObjectCommand,
  PutObjectCommand,
  S3Client,
} from '@aws-sdk/client-s3';
import { gql, makeExtendSchemaPlugin, makeWrapPlansPlugin } from 'postgraphile/utils';
import { lambda, sideEffect } from 'postgraphile/grafast';

const s3client = new S3Client({
  region: process.env.S3_REGION,
  endpoint: process.env.S3_ENDPOINT,
  forcePathStyle: true,
});
const bucketName = process.env.S3_BUCKET!;
const s3publicEndpoint = process.env.S3_PUBLIC_ENDPOINT || process.env.S3_ENDPOINT;

const plugins: GraphileConfig.Plugin[] = [
  makeWrapPlansPlugin({
    Mutation: {
      deleteAttachment(plan, _$source, $args) {
        const $result = plan();
        sideEffect($args.get(['input', 'objectName']), (Key) => {
          s3client.send(new DeleteObjectCommand({ Key, Bucket: bucketName }));
        })
        return $result;
      },
    },
  }),
  makeExtendSchemaPlugin((_build) => ({
    typeDefs: gql`
      extend type Attachment {
        uploadUrl: String!
        downloadUrl: String!
        publicUrl: String!
      }
    `,
    plans: {
      Attachment: {
        uploadUrl: ($parent) => lambda(
          $parent.get('object_name'),
          (objectName) => getSignedUrl(
            s3client,
            new PutObjectCommand({ Key: objectName as string, Bucket: bucketName }),
          )
        ),
        downloadUrl: ($parent) => lambda(
          $parent.get('object_name'),
          (objectName) => getSignedUrl(
            s3client,
            new GetObjectCommand({ Key: objectName as string, Bucket: bucketName }),
          )
        ),
        publicUrl: ($parent) => lambda(
          $parent.get('object_name'),
          (objectName) => `${s3publicEndpoint}/${bucketName}/${objectName}`,
        ),
      },
    },
  })),
];

export default plugins;
