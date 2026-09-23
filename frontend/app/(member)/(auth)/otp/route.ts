import { OtpLoginDocument } from '@/graphql/CurrentUser';
import { sanitizeReturnURL } from '@/lib/sanitize';
import { executeGraphql } from '@/lib/server/graphql';
import { setSessionCookie } from '@/lib/server/session';
import { NextResponse, type NextRequest } from 'next/server';

export async function GET(request: NextRequest) {
  try {
    const token = request.nextUrl.searchParams.get('token');
    const data = token ? await executeGraphql(OtpLoginDocument, { token }) : null;
    const result = data?.otpLogin?.result;
    if (!result?.jwt) {
      return NextResponse.redirect(new URL('/otp/invalid', request.url));
    }

    await setSessionCookie(result.jwt);
    const { origin, searchParams } = request.nextUrl;
    const destination = sanitizeReturnURL(searchParams.get('from'), origin) ?? '/dashboard';
    return NextResponse.redirect(new URL(destination, request.url));
  } catch {
    return NextResponse.redirect(new URL('/otp/invalid', request.url));
  }
}
