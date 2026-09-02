import { DeleteObjectCommand, S3Client } from '@aws-sdk/client-s3';
import type { Task } from 'graphile-worker';

const s3client = new S3Client({ forcePathStyle: true });
const Bucket = process.env.S3_BUCKET!;

const task: Task<'delete_file'> = async ({ object_key: Key }) => {
  await s3client.send(
    new DeleteObjectCommand({ Bucket, Key })
  );
};

export default task;
