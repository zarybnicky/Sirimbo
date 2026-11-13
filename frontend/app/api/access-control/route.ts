import { createHash } from 'node:crypto';
import { runQuery, withTransaction } from '@/lib/server/postgresql';
import { z } from 'zod';
import { accessControlContext, accessControlCredentials, insertAccessControlEvents, setAccessControlTenant } from './access-control.queries';

const EventBatch = z.object({
  events: z
    .array(
      z.object({
        id: z.string().trim().min(1).max(200),
        occurredAt: z.iso.datetime({ offset: true }),
        device: z.string().trim().min(1).max(200),
        kind: z.literal('mifare'),
        code: z
          .string()
          .trim()
          .regex(/^[0-9A-Fa-f]{8}$/),
        allowed: z.boolean(),
        reason: z.string().max(500).optional().default(''),
      }),
    )
    .min(1)
    .max(500),
});

async function accessContextFor(request: Request) {
  const bearer = request.headers.get('authorization')?.match(/^Bearer (\S+)$/i)?.[1];
  if (!bearer) return;

  const rows = await runQuery(accessControlContext, { token: bearer });
  return rows.length === 1
    ? { tenantId: rows[0]!.tenantId, locationId: '12' }
    : undefined;
}

function unauthorized() {
  return Response.json(
    { error: 'Unauthorized' },
    {
      status: 401,
      headers: { 'WWW-Authenticate': 'Bearer' },
    },
  );
}

export async function GET(request: Request) {
  const context = await accessContextFor(request);
  if (!context) return unauthorized();
  const { tenantId } = context;

  const rows = await withTransaction(async (client) => {
    await setAccessControlTenant.run({ tenantId }, client);
    return accessControlCredentials.run({ tenantId }, client);
  });
  const body = JSON.stringify({
    tenantId,
    credentials: rows.map((credential) => ({
      kind: credential.kind.toLowerCase(),
      code: credential.code,
      personId: credential.personId,
      personName: credential.personName,
    })),
  });
  const etag = `"${createHash('sha256').update(body).digest('hex')}"`;
  const matches = request.headers
    .get('if-none-match')
    ?.split(',')
    .some((value) => value.trim().replace(/^W\//, '') === etag || value.trim() === '*');
  return new Response(matches ? null : body, {
    status: matches ? 304 : 200,
    headers: {
      ETag: etag,
      'Cache-Control': 'private, no-cache',
      'Content-Type': 'application/json',
    },
  });
}

export async function POST(request: Request) {
  const context = await accessContextFor(request);
  if (!context) return unauthorized();
  const { tenantId } = context;

  let json: unknown;
  try {
    json = await request.json();
  } catch (error) {
    if (error instanceof SyntaxError) {
      return Response.json({ error: 'Invalid JSON' }, { status: 400 });
    }
    throw error;
  }
  const parsed = EventBatch.safeParse(json);
  if (!parsed.success) {
    return Response.json({ error: 'Invalid JSON schema' }, { status: 400 });
  }

  const events = parsed.data.events.map((event) => ({
    ...event,
    kind: 'MIFARE',
    code: event.code.toUpperCase(),
  }));
  const accepted = await withTransaction(async (client) => {
    await setAccessControlTenant.run({ tenantId }, client);
    const [result] = await insertAccessControlEvents.run({
      events: JSON.stringify(events),
      locationId: context.locationId,
    }, client);
    return result!.accepted;
  });

  return Response.json(
    { accepted, duplicates: events.length - accepted },
    { status: 202 },
  );
}
