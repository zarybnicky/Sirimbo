import { createHash } from 'node:crypto';
import { getPool, withTransaction } from '@/lib/server/postgresql';
import { z } from 'zod';

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

async function tenantIdFor(request: Request) {
  const bearer = request.headers.get('authorization')?.match(/^Bearer (\S+)$/i)?.[1];
  if (!bearer) return;

  const { rows } = await getPool().query<{ tenantId: string }>(
    `select tenant_id::text as "tenantId" from tenant_settings
     where settings->>'accessCredentialToken' = $1 limit 2`,
    [bearer],
  );
  return rows.length === 1 ? rows[0]?.tenantId : undefined;
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
  const tenantId = await tenantIdFor(request);
  if (!tenantId) return unauthorized();

  const { rows } = await withTransaction(async (client) => {
    await client.query("select set_config('jwt.claims.tenant_id', $1, true)", [tenantId]);
    return client.query<{
      kind: 'MIFARE';
      code: string;
      personId: string;
      personName: string;
    }>(
      `select c.kind, c.code, c.person_id::text as "personId", p.name as "personName"
       from access_credential c
       join person p on p.id = c.person_id
       where c.tenant_id = $1 and access_credential_is_allowed(c)
       order by c.id`,
      [tenantId],
    );
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
  const tenantId = await tenantIdFor(request);
  if (!tenantId) return unauthorized();

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
    await client.query("select set_config('jwt.claims.tenant_id', $1, true)", [tenantId]);
    const result = await client.query(
      `insert into access_event (
         external_id, device, kind, code, person_id, occurred_at, allowed, reason
       )
       select
         event.id,
         event.device,
         event.kind::access_credential_kind,
         event.code,
         (
           select credential.person_id
           from access_credential credential
           where credential.kind = event.kind::access_credential_kind
             and credential.code = event.code
             and credential.valid_range @> event."occurredAt"
         ),
         event."occurredAt",
         event.allowed,
         event.reason
       from jsonb_to_recordset($1::jsonb) as event(
         id text,
         device text,
         kind text,
         code text,
         "occurredAt" timestamptz,
         allowed boolean,
         reason text
       )
       on conflict (tenant_id, external_id) do nothing`,
      [JSON.stringify(events)],
    );
    return result.rowCount ?? 0;
  });

  return Response.json(
    { accepted, duplicates: events.length - accepted },
    { status: 202 },
  );
}
