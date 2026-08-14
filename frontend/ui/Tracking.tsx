'use client';

import { useTenantConfig } from '@/lib/auth';
import { usePathname, useSearchParams } from 'next/navigation';
import { GoogleAnalytics, pageView as googlePageView } from 'nextjs-google-analytics';
import React from 'react';
import type { init, pageView } from 'react-facebook-pixel';

export const Tracking = React.memo(function Tracking() {
  const lastTrackedPath = React.useRef<string | null>(null);
  const { facebookPixelId } = useTenantConfig();
  const facebookRef = React.useRef<{
    init: typeof init;
    pageView: typeof pageView;
  } | null>(null);

  React.useEffect(() => {
    if (process.env.NODE_ENV === 'development' || !facebookPixelId) return;
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

  const search = useSearchParams()?.toString() ?? '';
  const path = usePathname() + (search ? `?${search}` : '');

  React.useEffect(() => {
    if (process.env.NODE_ENV === 'development') return;
    if (lastTrackedPath.current === path) return;
    googlePageView({ path: path ?? undefined });
    facebookRef.current?.pageView();
    lastTrackedPath.current = path;
  }, [path]);

  return <GoogleAnalytics trackPageViews={false} />;
});
