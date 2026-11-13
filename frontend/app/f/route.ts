import { fileBucket, fileStorage } from '@/lib/server/file-storage';
import { withRequestPgClient } from '@/lib/server/postgresql';
import { sameOrigin } from '@/lib/server/session';
import { PutObjectCommand } from '@aws-sdk/client-s3';
import { Readable } from 'node:stream';
import type { ReadableStream } from 'node:stream/web';
import { type NextRequest, NextResponse } from 'next/server';
import { createUploadedFile, deleteUploadedFile, markFileUploaded, type ICreateUploadedFileResult } from './file.queries';

export async function POST(request: NextRequest) {
  if (!sameOrigin(request)) {
    return NextResponse.json({ error: 'Invalid origin' }, { status: 403 });
  }

  const encodedName = request.headers.get('x-file-name');
  if (!encodedName || !request.body) {
    return NextResponse.json({ error: 'Chybí soubor' }, { status: 400 });
  }

  let name: string;
  try {
    name = decodeURIComponent(encodedName);
  } catch {
    return NextResponse.json({ error: 'Neplatný název souboru' }, { status: 400 });
  }
  if (!name) {
    return NextResponse.json({ error: 'Chybí název souboru' }, { status: 400 });
  }

  const contentType = request.headers.get('content-type');
  const contentLength = request.headers.get('content-length');
  const byteSize =
    contentLength && /^\d+$/.test(contentLength) ? Number(contentLength) : undefined;

  let file: ICreateUploadedFileResult;
  try {
    file = await withRequestPgClient(async (client, settings) => {
      const objectKey = `${settings['jwt.claims.tenant_id']}/${Date.now()}-${name}`;
      const [result] = await createUploadedFile.run({
        objectKey,
        name,
        contentType,
        byteSize,
      }, client);
      return result!;
    });
  } catch (error) {
    if ((error as { code?: string }).code === '42501') {
      return NextResponse.json(
        { error: 'Nemáte oprávnění nahrávat soubory' },
        { status: 403 },
      );
    }
    console.error('Failed to create file record', error);
    return NextResponse.json({ error: 'Soubor se nepodařilo nahrát' }, { status: 500 });
  }

  try {
    await fileStorage.send(
      new PutObjectCommand({
        Bucket: fileBucket,
        Key: file.object_key,
        Body: Readable.fromWeb(request.body as ReadableStream),
        ContentType: contentType ?? undefined,
        ContentLength: Number.isSafeInteger(byteSize) ? byteSize : undefined,
      }),
    );
    await withRequestPgClient(async (client) => {
      await markFileUploaded.run({ id: file.id }, client);
    });
  } catch (error) {
    await withRequestPgClient(async (client) => {
      await deleteUploadedFile.run({ id: file.id }, client);
    }).catch((cleanupError: unknown) => {
      console.error('Failed to clean up file record', cleanupError);
    });
    console.error('Failed to store file', error);
    return NextResponse.json({ error: 'Soubor se nepodařilo nahrát' }, { status: 502 });
  }

  return NextResponse.json({ id: file.id, name, url: file.url }, { status: 201 });
}
