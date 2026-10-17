import { runQuery } from '@/lib/server/postgresql';
import { getRequestContext } from '@/lib/server/tenant';
import { getTenant } from '@/tenant/catalog';
import { NextResponse, type NextRequest } from 'next/server';
import { createHandoffOtp, handoffEvent } from './handoff.queries';

export async function GET(request: NextRequest) {
  const { auth, tenant } = await getRequestContext();
  const origin =
    process.env.NODE_ENV === 'development'
      ? request.nextUrl.origin
      : tenant.config.origin;
  const eventId = request.nextUrl.searchParams.get('event');
  if (!eventId || !/^\d{1,18}$/.test(eventId)) {
    return NextResponse.redirect(new URL('/otp/invalid', origin));
  }

  const [event] = await runQuery(handoffEvent, { eventId });
  const destination = getTenant(event?.tenant_id);
  if (!destination) return NextResponse.redirect(new URL('/otp/invalid', origin));

  if (!auth.userId) {
    const login = new URL('/login', origin);
    login.searchParams.set('from', `/api/auth/handoff?event=${eventId}`);
    return NextResponse.redirect(login);
  }

  const [otp] = await runQuery(createHandoffOtp, {
    userId: auth.userId,
    tenantId: destination.id,
    eventId,
  });
  const callback = new URL(
    '/otp',
    destination.id === tenant.id ? origin : destination.config.origin,
  );
  callback.searchParams.set('token', otp!.access_token);
  return NextResponse.redirect(callback, {
    headers: { 'Referrer-Policy': 'no-referrer', 'Cache-Control': 'no-store' },
  });
}
