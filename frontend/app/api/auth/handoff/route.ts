import { runQuery } from '@/lib/server/postgresql';
import { getRequestContext, getRequestHostTenant } from '@/lib/server/tenant';
import { clearTenantCookie } from '@/lib/server/session';
import { sanitizeReturnURL } from '@/lib/sanitize';
import { getTenant } from '@/tenant/catalog';
import { NextResponse, type NextRequest } from 'next/server';
import { createHandoffOtp, handoffEvent } from '@/lib/auth.queries';

export async function GET(request: NextRequest) {
  const { auth, tenant } = await getRequestContext();
  const params = request.nextUrl.searchParams;
  const switchingClub = params.has('tenant');
  const source = switchingClub ? await getRequestHostTenant() : tenant;
  const development = process.env.NODE_ENV === 'development';
  const origin = development ? request.nextUrl.origin : source.config.origin;
  const eventId = params.get('event');
  const [event] = !switchingClub && eventId && /^\d{1,18}$/.test(eventId)
    ? await runQuery(handoffEvent, { eventId })
    : [];
  const destination = switchingClub
    ? getTenant(params.get('tenant'))
    : getTenant(event?.tenant_id);
  if (!destination) return NextResponse.redirect(new URL('/otp/invalid', origin));

  const destinationOrigin = development && switchingClub
    ? origin
    : destination.config.origin;
  const from = sanitizeReturnURL(params.get('from'), destinationOrigin) ?? '/dashboard';
  let redirectTo: URL;
  if (!auth.userId) {
    redirectTo = new URL('/login', origin);
    redirectTo.searchParams.set('from', request.nextUrl.pathname + request.nextUrl.search);
  } else if (switchingClub && development) {
    redirectTo = new URL(from, origin);
  } else {
    const [otp] = await runQuery(createHandoffOtp, {
      userId: auth.userId,
      tenantId: destination.id,
      intent: switchingClub ? 'tenant_handoff' : 'event_handoff',
      eventId: switchingClub ? null : eventId,
    });
    redirectTo = new URL('/otp', destination.id === source.id ? origin : destinationOrigin);
    redirectTo.searchParams.set('token', otp!.access_token);
    if (switchingClub) redirectTo.searchParams.set('from', from);
  }

  const response = NextResponse.redirect(redirectTo, {
    headers: { 'Referrer-Policy': 'no-referrer', 'Cache-Control': 'no-store' },
  });
  if (switchingClub) {
    if (development) {
      response.cookies.set('tenant_id', String(destination.id), { path: '/', sameSite: 'lax' });
    } else {
      clearTenantCookie(request, response);
    }
  }
  return response;
}
