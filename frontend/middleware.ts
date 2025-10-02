import { NextResponse } from "next/server"
import type { NextRequest } from "next/server"

export const config = { matcher: ["/:path*"] }

export function middleware(request: NextRequest) {
  const host = request.headers.get("x-forwarded-host") ?? request.nextUrl.host

  if (!host) {
    return NextResponse.next()
  }

  const tenantHost = host.toLowerCase()
  const requestHeaders = new Headers(request.headers)
  requestHeaders.set("x-tenant-host", tenantHost)

  return NextResponse.next({
    request: {
      headers: requestHeaders,
    },
  })
}
