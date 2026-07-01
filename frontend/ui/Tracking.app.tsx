'use client';

import { tenantConfigAtom } from '@/ui/state/auth';
import { useAtomValue } from 'jotai';
import { usePathname, useSearchParams } from 'next/navigation';
import { GoogleAnalytics } from 'nextjs-google-analytics';
import React from 'react';
import type { init, pageView as facebookPageView } from 'react-facebook-pixel';

export const Tracking = React.memo(function Tracking() {
  const { facebookPixelId } = useAtomValue(tenantConfigAtom);
  const lastTrackedPath = React.useRef<string>(undefined);
  const facebookRef = React.useRef<{
    init: typeof init;
    pageView: typeof facebookPageView;
  } | null>(null);

  const pathname = usePathname() || '/';
  const search = useSearchParams()?.toString() ?? '';
  const path = search ? `${pathname}?${search}` : pathname;

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

  React.useEffect(() => {
    if (process.env.NODE_ENV === 'development') return;
    if (lastTrackedPath.current === path) return;
    lastTrackedPath.current = path;
    facebookRef.current?.pageView();
  }, [path]);

  return <GoogleAnalytics trackPageViews={{ ignoreHashChange: true }} />;
});
