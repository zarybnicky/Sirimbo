/* eslint-disable import-x/no-unused-modules */
import { fileBucket, fileStorage } from '@/lib/server/file-storage';
import { withRequestPgClient } from '@/lib/server/postgresql';
import { sameOrigin } from '@/lib/server/session';
import { PutObjectCommand } from '@aws-sdk/client-s3';
import { Readable } from 'node:stream';
import type { ReadableStream } from 'node:stream/web';
import { NextResponse, type NextRequest } from 'next/server';

export const runtime = 'nodejs';

type FileRow = {
  id: string;
  object_key: string;
  url: string;
};

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
  const byteSize = contentLength && /^\d+$/.test(contentLength) ? Number(contentLength) : undefined;

  let file: FileRow;
  try {
    file = await withRequestPgClient(async (client, settings) => {
      const objectKey = `${settings['jwt.claims.tenant_id']}/${Date.now()}-${name}`;
      const result = await client.query<FileRow>(
        `insert into file (object_key, name, content_type, byte_size)
         values ($1, $2, $3, $4)
         returning id, object_key, url`,
        [objectKey, name, contentType, byteSize],
      );
      return result.rows[0]!;
    });
  } catch (error) {
    if ((error as { code?: string }).code === '42501') {
      return NextResponse.json({ error: 'Nemáte oprávnění nahrávat soubory' }, { status: 403 });
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
      await client.query(`update file set uploaded_at = now() where id = $1`, [file.id]);
    });
  } catch (error) {
    await withRequestPgClient(async (client) => {
      await client.query(`delete from file where id = $1`, [file.id]);
    }).catch((cleanupError: unknown) => {
      console.error('Failed to clean up file record', cleanupError);
    });
    console.error('Failed to store file', error);
    return NextResponse.json({ error: 'Soubor se nepodařilo nahrát' }, { status: 502 });
  }

  return NextResponse.json(
    {
      id: file.id,
      name,
      url: file.url,
    },
    { status: 201 },
  );
}
