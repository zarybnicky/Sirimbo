import { getSignedUrl } from '@aws-sdk/s3-request-presigner';
import { PutObjectCommand, S3Client } from '@aws-sdk/client-s3';
import { gql, extendSchema } from 'postgraphile/utils';
import { lambda } from 'postgraphile/grafast';

const s3client = new S3Client({
  region: process.env.S3_REGION,
  endpoint: process.env.S3_ENDPOINT,
  forcePathStyle: true,
});
const bucketName = process.env.S3_BUCKET!;
const s3publicEndpoint = process.env.S3_PUBLIC_ENDPOINT || process.env.S3_ENDPOINT;

const plugins: GraphileConfig.Plugin[] = [
  extendSchema((_build) => ({
    typeDefs: gql`
      extend type Attachment {
        uploadUrl: String!
        publicUrl: String!
      }
    `,
    plans: {
      Attachment: {
        uploadUrl: ($parent: any) =>
          lambda($parent.get('object_name'), (objectName) =>
            getSignedUrl(
              s3client,
              new PutObjectCommand({ Key: objectName as string, Bucket: bucketName }),
            ),
          ),
        publicUrl: ($parent: any) =>
          lambda(
            $parent.get('object_name'),
            (objectName) => `${s3publicEndpoint}/${bucketName}/${objectName}`,
          ),
      },
    },
  })),
];

export default plugins;
