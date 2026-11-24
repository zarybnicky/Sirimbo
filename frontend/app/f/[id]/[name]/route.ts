import { fileBucket, fileStorage } from '@/app/f/file-storage.ts';
import { withRequestPgClient } from '@/lib/server/postgresql';
import { imageWidths } from '@/lib/image-presets';
import { findDownloadFile, type IFindDownloadFileResult } from '../../file.queries';
import {
  GetObjectCommand,
  type GetObjectCommandInput,
  type GetObjectCommandOutput,
  HeadObjectCommand,
  PutObjectCommand,
  type S3ServiceException,
} from '@aws-sdk/client-s3';
import sharp from 'sharp';

export const runtime = 'nodejs';

const cacheHeaders = { 'Cache-Control': 'private, max-age=3600', Vary: 'Cookie' };

function requestDate(request: Request, name: string) {
  const value = request.headers.get(name);
  if (!value) return;
  const date = new Date(value);
  return Number.isNaN(date.valueOf()) ? undefined : date;
}

function objectInput(
  request: Request,
  file: IFindDownloadFileResult,
): GetObjectCommandInput {
  return {
    Bucket: fileBucket,
    Key: file.object_key,
    IfMatch: request.headers.get('if-match') ?? undefined,
    IfModifiedSince: requestDate(request, 'if-modified-since'),
    IfNoneMatch: request.headers.get('if-none-match') ?? undefined,
    IfUnmodifiedSince: requestDate(request, 'if-unmodified-since'),
  };
}

function responseHeaders(file: IFindDownloadFileResult, object: GetObjectCommandOutput) {
  const encodedName = encodeURIComponent(file.name)
    .replaceAll("'", '%27')
    .replaceAll('*', '%2A');
  const headers = new Headers({
    ...cacheHeaders,
    'Accept-Ranges': 'bytes',
    'Content-Disposition': `inline; filename*=UTF-8''${encodedName}`,
  });
  const contentType = object.ContentType ?? file.content_type;
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

function unsatisfiedRange(byteSize: string | null) {
  const headers = new Headers({ 'Accept-Ranges': 'bytes' });
  if (byteSize) headers.set('Content-Range', `bytes */${byteSize}`);
  return new Response(null, { status: 416, headers });
}

function errorResponse(error: unknown, byteSize: string | null) {
  const status = (error as S3ServiceException).$metadata?.httpStatusCode;
  if (status === 416) return unsatisfiedRange(byteSize);
  if ([304, 412].includes(status ?? 0)) {
    return new Response(null, {
      status,
      headers: { ...(status === 304 ? cacheHeaders : {}), 'Accept-Ranges': 'bytes' },
    });
  }
  if (status === 404) return new Response(null, { status: 404 });

  console.error('Failed to read file', error);
  return new Response(null, { status: 502 });
}

async function findFile(context: RouteContext<'/f/[id]/[name]'>) {
  const { id } = await context.params;
  if (!/^\d+$/.test(id)) return;

  return withRequestPgClient(async (client) => {
    const [file] = await findDownloadFile.run({ id }, client);
    return file;
  });
}

async function imageThumbnail(input: Uint8Array, width: number) {
  const image = sharp(input);
  const metadata = await image.metadata();
  if (metadata.format === 'svg' || (metadata.pages ?? 1) > 1) return;

  if (metadata.format === 'png') {
    const png = Buffer.from(input.buffer, input.byteOffset, input.byteLength);
    for (
      let offset = 8;
      offset + 12 <= png.length;
      offset += 12 + png.readUInt32BE(offset)
    ) {
      const type = png.toString('ascii', offset + 4, offset + 8);
      if (type === 'acTL') return;
      if (type === 'IDAT') break;
    }
  }

  return image
    .autoOrient()
    .resize({ width, withoutEnlargement: true })
    .webp({ quality: 75 })
    .toBuffer();
}

export async function GET(request: Request, context: RouteContext<'/f/[id]/[name]'>) {
  const file = await findFile(context);
  if (!file) return new Response(null, { status: 404 });
  const originalKey = file.object_key;

  const widthParam = new URL(request.url).searchParams.get('w');
  const width = widthParam === null ? undefined : Number(widthParam);
  if (width !== undefined && !imageWidths.includes(width)) {
    return new Response('Unsupported image width', { status: 400 });
  }

  const thumbnailKey =
    width !== undefined &&
    /^image\/(?:jpeg|png|webp|avif|tiff|gif)$/.test(file.content_type ?? '')
      ? `thumbnails/${originalKey}/v1/${width}.webp`
      : undefined;
  const head = request.method === 'HEAD';
  const Command = head ? HeadObjectCommand : GetObjectCommand;
  const byteSize = thumbnailKey ? null : file.byte_size;

  async function readObject(
    input: GetObjectCommandInput,
  ): Promise<GetObjectCommandOutput> {
    try {
      return await fileStorage.send(
        new Command({ ...input, Key: thumbnailKey ?? originalKey }),
      );
    } catch (error) {
      if (
        !thumbnailKey ||
        (error as S3ServiceException).$metadata?.httpStatusCode !== 404
      )
        throw error;
    }

    const source = await fileStorage.send(
      new GetObjectCommand({ Bucket: fileBucket, Key: originalKey }),
    );
    if (!source.Body) throw new Error('File storage returned no body');
    const body = await imageThumbnail(await source.Body.transformToByteArray(), width!);
    if (body) {
      await fileStorage.send(
        new PutObjectCommand({
          Bucket: fileBucket,
          Key: thumbnailKey,
          ContentType: 'image/webp',
          Body: body,
        }),
      );
    }
    return fileStorage.send(
      new Command({ ...input, Key: body ? thumbnailKey : originalKey }),
    );
  }

  const range = head ? null : request.headers.get('range');
  if (range && !/^bytes=(?:\d+-\d*|-\d+)$/.test(range)) return unsatisfiedRange(byteSize);
  const input = objectInput(request, file);
  const ifRange = range && request.headers.get('if-range');
  const ifRangeDate =
    ifRange && !ifRange.startsWith('"') ? requestDate(request, 'if-range') : undefined;
  if (range && (!ifRange || ifRange.startsWith('"') || ifRangeDate)) input.Range = range;
  if (ifRange?.startsWith('"')) input.IfMatch = ifRange;
  else if (ifRangeDate) input.IfUnmodifiedSince = ifRangeDate;

  try {
    const object = await readObject(input).catch((error: unknown) => {
      if (!ifRange || (error as S3ServiceException).$metadata?.httpStatusCode !== 412)
        throw error;
      return readObject(objectInput(request, file));
    });

    if (!head && !object.Body) return new Response(null, { status: 502 });
    return new Response(head ? null : object.Body!.transformToWebStream(), {
      status: object.ContentRange ? 206 : 200,
      headers: responseHeaders(file, object),
    });
  } catch (error) {
    return errorResponse(error, byteSize);
  }
}

export const HEAD = GET;
