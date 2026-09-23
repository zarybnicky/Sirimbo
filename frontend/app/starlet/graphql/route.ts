import { getRequestContext } from '@/lib/server/tenant';
import { sameOrigin } from '@/lib/server/session';
import type { NextRequest } from 'next/server';

const UPSTREAM = 'https://evidence.tsstarlet.com';

export const dynamic = 'force-dynamic';

export async function POST(req: NextRequest) {
  if (!sameOrigin(req)) {
    return Response.json({ error: 'Invalid origin' }, { status: 403 });
  }

  const { auth, tenant } = await getRequestContext();
  if (!tenant.config.enableStarletImport || !auth.isAdmin) {
    return Response.json({ error: 'Forbidden' }, { status: 403 });
  }

  const body = await req.json().catch(() => null);

  const isLogin = !!body && typeof body === 'object' && body.query === '';
  const path = isLogin ? '/spa_auth/login' : '/graphql';
  const payload = isLogin ? body.variables : body;

  const token = req.headers.get('authorization')?.replace(/^Bearer\s+/i, '').trim() ?? '';

  const upstream = await fetch(UPSTREAM + path, {
    method: 'POST',
    headers: {
      'content-type': 'application/json',
      cookie: token ? `auth=${encodeURIComponent(token)}` : '',
      origin: UPSTREAM,
      referer: UPSTREAM,
    },
    body: JSON.stringify(payload),
    redirect: 'manual',
  });

  const headers = new Headers();
  const contentType = upstream.headers.get('content-type');
  if (contentType) headers.set('content-type', contentType);

  return new Response(upstream.body, { status: upstream.status, headers });
}
