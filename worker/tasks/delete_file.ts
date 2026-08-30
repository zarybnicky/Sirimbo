import { DeleteObjectCommand, S3Client } from '@aws-sdk/client-s3';
import type { Task } from 'graphile-worker';

const fileStorage = new S3Client({
  region: process.env.S3_REGION,
  endpoint: process.env.S3_ENDPOINT,
  forcePathStyle: true,
});

const task: Task<'delete_file'> = async ({ object_key }) => {
  await fileStorage.send(
    new DeleteObjectCommand({
      Bucket: process.env.S3_BUCKET!,
      Key: object_key,
    }),
  );
};

export default task;
