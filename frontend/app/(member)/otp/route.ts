/* eslint-disable import-x/no-unused-modules */
import { OtpLoginDocument } from '@/graphql/CurrentUser';
import { executeGraphql } from '@/lib/server/graphql';
import { setSessionCookie } from '@/lib/server/session';
import { getRequestTenant } from '@/tenant/server';
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
    const tenant = await getRequestTenant();
    const from = request.nextUrl.searchParams.get('from');
    const destination = !result.usr?.userProxiesList.length
      ? '/profil'
      : from || (tenant.config.publicSite ? '/dashboard' : '/rozpis');
    return NextResponse.redirect(new URL(destination, request.url));
  } catch {
    return NextResponse.redirect(new URL('/otp/invalid', request.url));
  }
}
