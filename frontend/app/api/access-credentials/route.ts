import { createHash } from 'node:crypto';
import { getPool, withTransaction } from '@/lib/server/postgresql';

export async function GET(request: Request) {
  const headers = { 'Cache-Control': 'private, no-store' };
  const bearer = request.headers.get('authorization')?.match(/^Bearer (\S+)$/i)?.[1];
  const { rows: tenants } = bearer
    ? await getPool().query<{ tenantId: string }>(
        `select tenant_id::text as "tenantId" from tenant_settings
           where settings->>'accessCredentialToken' = $1 limit 2`,
        [bearer],
      )
    : { rows: [] };
  const tenantId = tenants.length === 1 ? tenants[0]?.tenantId : undefined;
  if (!tenantId) {
    return Response.json(
      { error: 'Unauthorized' },
      {
        status: 401,
        headers: { ...headers, 'WWW-Authenticate': 'Bearer' },
      },
    );
  }

  const { rows } = await withTransaction(async (client) => {
    await client.query("select set_config('jwt.claims.tenant_id', $1, true)", [tenantId]);
    return client.query<{ id: string; uid: string }>(
      `select c.id::text, c.uid from access_credential c
         where c.tenant_id = $1 and access_credential_is_allowed(c)
         order by c.id`,
      [tenantId],
    );
  });
  const body = JSON.stringify({ tenantId, credentials: rows });
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
