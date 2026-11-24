import { DevelopmentUsersDocument } from '@/graphql/CurrentUser';
import { logInAsAction } from '@/lib/auth-actions';
import { getDevelopmentUsers, pickDevelopmentUser } from '@/lib/development-identity';
import { sanitizeReturnURL } from '@/lib/sanitize';
import { executeGraphql } from '@/lib/server/graphql';
import { getRequestContext } from '@/lib/server/tenant';
import { NextResponse, type NextRequest } from 'next/server';

export async function GET(request: NextRequest) {
  if (process.env.NODE_ENV !== 'development') {
    return new Response(null, { status: 404 });
  }

  const { origin, searchParams } = request.nextUrl;
  const role = searchParams.get('role');

  const { auth, pgSettings, tenant } = await getRequestContext();
  const { users } = await executeGraphql(DevelopmentUsersDocument, undefined, {
    pgSettings: { ...pgSettings, role: 'system_admin' },
  });
  const candidates = getDevelopmentUsers(users?.nodes ?? [], tenant, Date.now());
  const user = pickDevelopmentUser(candidates.filter((x) => x.role === role), auth.userId);
  if (!user)
    return new Response('No development user found for this role', { status: 404 });

  const result = await logInAsAction(user.id);
  if ('error' in result) return new Response(result.error, { status: 400 });

  const from = sanitizeReturnURL(searchParams.get('from'), origin) ?? result.redirectTo;
  return NextResponse.redirect(new URL(from, origin), {
    headers: { 'Cache-Control': 'no-store' },
  });
}
