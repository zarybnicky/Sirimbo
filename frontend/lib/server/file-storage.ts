import 'server-only';

import { S3Client } from '@aws-sdk/client-s3';

export const fileBucket = process.env.S3_BUCKET!;
export const fileStorage = new S3Client({ forcePathStyle: true });
