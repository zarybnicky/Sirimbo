'use client';

import React from 'react';
import { useRouter } from 'next/compat/router';
import { usePathname, useSearchParams } from 'next/navigation';
import { GoogleAnalytics, pageView as googlePageView } from 'nextjs-google-analytics';
import type { init, pageView } from 'react-facebook-pixel';
import { useAtomValue } from 'jotai';
import { tenantConfigAtom } from './state/auth';

export const Tracking = React.memo(function Tracking() {
  const router = useRouter();
  const pathname = usePathname();
  const searchParams = useSearchParams();
  const lastTrackedPath = React.useRef<string | null>(null);
  const { facebookPixelId } = useAtomValue(tenantConfigAtom);
  const facebookRef = React.useRef<{
    init: typeof init;
    pageView: typeof pageView;
  } | null>(null);

  React.useEffect(() => {
    if (process.env.NODE_ENV === 'development') return;
    if (!facebookPixelId) return;
    let disposed = false;
    (async () => {
      const facebook = await import('react-facebook-pixel').then((x) => x.default);
      if (disposed) return;
      facebook.init(facebookPixelId);
      facebook.pageView();
      facebookRef.current = facebook;
    })();
    return () => {
      disposed = true;
      facebookRef.current = null;
    };
  }, [facebookPixelId]);

  const track = React.useCallback(
    (path: string) => {
      if (process.env.NODE_ENV === 'development') return;
      if (!facebookPixelId) return;
      if (path === lastTrackedPath.current) return;
      lastTrackedPath.current = path;
      facebookRef.current?.pageView();
    },
    [facebookPixelId],
  );

  React.useEffect(() => {
    if (!router) return;
    router.events.on('routeChangeComplete', track);
    return () => router.events.off('routeChangeComplete', track);
  }, [track, router]);

  const search = searchParams?.toString() ?? '';
  const path = search ? `${pathname}?${search}` : pathname;

  React.useEffect(() => {
    if (router) return;
    if (process.env.NODE_ENV === 'development') return;
    if (lastTrackedPath.current === undefined) {
      lastTrackedPath.current = path;
      return;
    }
    if (lastTrackedPath.current === path) return;
    lastTrackedPath.current = path;
    googlePageView({ path: path ?? undefined });
    facebookRef.current?.pageView();
  }, [path, router]);

  return (
    <GoogleAnalytics
      trackPageViews={router ? { ignoreHashChange: true } : false}
    />
  );
});
