import { getSignedUrl } from '@aws-sdk/s3-request-presigner';
import { PutObjectCommand, S3Client } from '@aws-sdk/client-s3';
import { gql, extendSchema } from 'postgraphile/utils';
import { lambda } from 'grafast';

const s3client = new S3Client({ forcePathStyle: true });
const Bucket = process.env.S3_BUCKET!;

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
          lambda($parent.get('object_name'), (Key) =>
            getSignedUrl(
              s3client,
              new PutObjectCommand({ Key: Key as string, Bucket }),
            ),
          ),
      },
    },
  })),
];

export default plugins;
