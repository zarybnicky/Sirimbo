import { DeleteObjectCommand, ListObjectsV2Command, S3Client } from '@aws-sdk/client-s3';
import type { Task } from 'graphile-worker';

const s3client = new S3Client({ forcePathStyle: true });
const Bucket = process.env.S3_BUCKET!;

const task: Task<'delete_file'> = async ({ object_key: Key }) => {
  await s3client.send(
    new DeleteObjectCommand({ Bucket, Key })
  );

  let ContinuationToken: string | undefined;
  do {
    const page = await s3client.send(new ListObjectsV2Command({
      Bucket,
      Prefix: `thumbnails/${Key}/`,
      ContinuationToken,
    }));
    for (const object of page.Contents ?? []) {
      if (object.Key) await s3client.send(new DeleteObjectCommand({ Bucket, Key: object.Key }));
    }
    ContinuationToken = page.NextContinuationToken;
  } while (ContinuationToken);
};

export default task;
