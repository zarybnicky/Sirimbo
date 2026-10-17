import { OtpLoginDocument } from '@/graphql/CurrentUser';
import { sanitizeReturnURL } from '@/lib/sanitize';
import { executeGraphql } from '@/lib/server/graphql';
import { runQuery } from '@/lib/server/postgresql';
import { setSessionCookie } from '@/lib/server/session';
import { getRequestContext } from '@/lib/server/tenant';
import { NextResponse, type NextRequest } from 'next/server';
import { otpDestination } from './otp.queries';

export async function GET(request: NextRequest) {
  const { tenant } = await getRequestContext();
  const origin =
    process.env.NODE_ENV === 'development'
      ? request.nextUrl.origin
      : tenant.config.origin;
  let destination = new URL('/otp/invalid', origin);
  try {
    const token = request.nextUrl.searchParams.get('token');
    if (!token) throw new Error('Missing OTP');

    const [otp] = await runQuery(otpDestination, { token });
    if (!otp) throw new Error('Invalid OTP');

    if (otp.intent === 'event_handoff') {
      if (String(tenant.id) !== otp.tenant_id) throw new Error('Wrong tenant');
      destination = new URL(
        `/termin/${otp.event_id}?registration=preview&signup=1`,
        origin,
      );
    }

    const data = await executeGraphql(OtpLoginDocument, { token });
    const result = data.otpLogin?.result;
    if (!result?.jwt) throw new Error('Invalid OTP');
    await setSessionCookie(result.jwt);

    if (otp.intent !== 'event_handoff') {
      destination = new URL(
        sanitizeReturnURL(request.nextUrl.searchParams.get('from'), origin) ?? '/profil',
        origin,
      );
    }
    return NextResponse.redirect(destination, {
      headers: { 'Referrer-Policy': 'no-referrer', 'Cache-Control': 'no-store' },
    });
  } catch {
    if (destination.pathname !== '/otp/invalid')
      destination.searchParams.set('login', 'failed');
    return NextResponse.redirect(destination, {
      headers: { 'Referrer-Policy': 'no-referrer', 'Cache-Control': 'no-store' },
    });
  }
}
