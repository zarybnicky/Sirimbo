/* eslint-disable import-x/no-unused-modules */
import { fileBucket, fileStorage } from '@/lib/server/file-storage';
import { withRequestPgClient } from '@/lib/server/postgresql';
import {
  GetObjectCommand,
  HeadObjectCommand,
  type GetObjectCommandInput,
  type GetObjectCommandOutput,
  type HeadObjectCommandOutput,
} from '@aws-sdk/client-s3';

export const runtime = 'nodejs';

type FileRow = {
  object_key: string;
  name: string;
  content_type: string | null;
  byte_size: string | null;
};

type ObjectMetadata = Pick<
  GetObjectCommandOutput | HeadObjectCommandOutput,
  'ContentLength' | 'ContentRange' | 'ContentType' | 'ETag' | 'LastModified'
>;

function requestDate(request: Request, name: string) {
  const value = request.headers.get(name);
  if (!value) return;
  const date = new Date(value);
  return Number.isNaN(date.valueOf()) ? undefined : date;
}

function objectInput(request: Request, file: FileRow): GetObjectCommandInput {
  return {
    Bucket: fileBucket,
    Key: file.object_key,
    IfMatch: request.headers.get('if-match') ?? undefined,
    IfModifiedSince: requestDate(request, 'if-modified-since'),
    IfNoneMatch: request.headers.get('if-none-match') ?? undefined,
    IfUnmodifiedSince: requestDate(request, 'if-unmodified-since'),
  };
}

function responseHeaders(file: FileRow, object: ObjectMetadata) {
  const encodedName = encodeURIComponent(file.name)
    .replaceAll("'", '%27')
    .replaceAll('*', '%2A');
  const headers = new Headers({
    'Accept-Ranges': 'bytes',
    'Cache-Control': 'private, no-store',
    'Content-Disposition': `inline; filename*=UTF-8''${encodedName}`,
  });
  const contentType = file.content_type ?? object.ContentType;
  if (contentType) headers.set('Content-Type', contentType);
  if (object.ContentLength !== undefined) {
    headers.set('Content-Length', String(object.ContentLength));
  }
  if (object.ContentRange) headers.set('Content-Range', object.ContentRange);
  if (object.ETag) headers.set('ETag', object.ETag);
  if (object.LastModified)
    headers.set('Last-Modified', object.LastModified.toUTCString());
  return headers;
}

function unsatisfiedRange(file: FileRow) {
  const headers = new Headers({ 'Accept-Ranges': 'bytes' });
  if (file.byte_size) headers.set('Content-Range', `bytes */${file.byte_size}`);
  return new Response(null, { status: 416, headers });
}

function errorResponse(error: unknown, file: FileRow) {
  const status = (error as { $metadata?: { httpStatusCode?: number } }).$metadata
    ?.httpStatusCode;
  if (status === 416) return unsatisfiedRange(file);
  if ([304, 412].includes(status ?? 0)) {
    return new Response(null, { status, headers: { 'Accept-Ranges': 'bytes' } });
  }
  if (status === 404) return new Response(null, { status: 404 });

  console.error('Failed to read file', error);
  return new Response(null, { status: 502 });
}

async function findFile(context: RouteContext<'/f/[id]/[name]'>) {
  const { id } = await context.params;
  if (!/^\d+$/.test(id)) return;

  return withRequestPgClient(async (client) => {
    const result = await client.query<FileRow>(
      `select object_key, name, content_type, byte_size from file where id = $1`,
      [id],
    );
    return result.rows[0];
  });
}

export async function GET(request: Request, context: RouteContext<'/f/[id]/[name]'>) {
  const file = await findFile(context);
  if (!file) return new Response(null, { status: 404 });

  const input = objectInput(request, file);
  const range = request.headers.get('range');
  if (range && !/^bytes=(?:\d+-\d*|-\d+)$/.test(range)) return unsatisfiedRange(file);

  const ifRange = range && request.headers.get('if-range');
  const ifRangeDate =
    ifRange && !ifRange.startsWith('"') ? requestDate(request, 'if-range') : undefined;
  if (range && (!ifRange || ifRange.startsWith('"') || ifRangeDate)) input.Range = range;
  if (ifRange?.startsWith('"')) input.IfMatch = ifRange;
  else if (ifRangeDate) input.IfUnmodifiedSince = ifRangeDate;

  let object: GetObjectCommandOutput;
  try {
    object = await fileStorage.send(new GetObjectCommand(input));
  } catch (error) {
    const status = (error as { $metadata?: { httpStatusCode?: number } }).$metadata
      ?.httpStatusCode;
    if (!ifRange || status !== 412) return errorResponse(error, file);

    try {
      object = await fileStorage.send(new GetObjectCommand(objectInput(request, file)));
    } catch (retryError) {
      return errorResponse(retryError, file);
    }
  }
  if (!object.Body) return new Response(null, { status: 502 });

  return new Response(object.Body.transformToWebStream(), {
    status: object.ContentRange ? 206 : 200,
    headers: responseHeaders(file, object),
  });
}

export async function HEAD(request: Request, context: RouteContext<'/f/[id]/[name]'>) {
  const file = await findFile(context);
  if (!file) return new Response(null, { status: 404 });

  try {
    const object = await fileStorage.send(
      new HeadObjectCommand(objectInput(request, file)),
    );
    return new Response(null, { headers: responseHeaders(file, object) });
  } catch (error) {
    return errorResponse(error, file);
  }
}
